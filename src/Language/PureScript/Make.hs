module Language.PureScript.Make
  (
  -- * Make API
  rebuildModule
  , rebuildModule'
  , make
  , inferForeignModules
  , module Monad
  , module Actions
  ) where

import           Prelude

import           Control.Concurrent.Lifted as C
import           Control.Exception.Base (onException)
import           Control.Monad hiding (sequence)
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.IO.Class
import           Control.Monad.Supply
import           Control.Monad.Trans.Control (MonadBaseControl(..), control)
import           Control.Monad.Trans.State (runStateT)
import           Control.Monad.Writer.Class (MonadWriter(..), censor)
import           Control.Monad.Writer.Strict (runWriterT)
import           Data.Function (on)
import           Data.Foldable (fold, for_)
import           Data.List (foldl', sortOn)
import qualified Data.List.NonEmpty as NEL
import           Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Language.PureScript.AST
import           Language.PureScript.Crash
import qualified Language.PureScript.CST as CST
import qualified Language.PureScript.Docs.Convert as Docs
import           Language.PureScript.Environment
import           Language.PureScript.Errors
import           Language.PureScript.Externs
import           Language.PureScript.Linter
import           Language.PureScript.ModuleDependencies
import           Language.PureScript.Names
import           Language.PureScript.Renamer
import           Language.PureScript.Sugar
import           Language.PureScript.TypeChecker
import           Language.PureScript.Make.BuildPlan
import qualified Language.PureScript.Make.BuildPlan as BuildPlan
import qualified Language.PureScript.Make.Cache as Cache
import           Language.PureScript.Make.Actions as Actions
import           Language.PureScript.Make.Monad as Monad
import qualified Language.PureScript.CoreFn as CF
import           System.Directory (doesFileExist)
import           System.FilePath (replaceExtension)
import           System.Environment (lookupEnv)
import Debug.Trace
import System.IO.Unsafe (unsafePerformIO)
import PrettyPrint


-- purserl
import Control.Applicative ((<|>))
import qualified Build as Erl.Build
-- import System.IO.Unsafe (unsafePerformIO)
--


-- | Rebuild a single module.
--
-- This function is used for fast-rebuild workflows (PSCi and psc-ide are examples).
rebuildModule
  :: forall m
   . (MonadBaseControl IO m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => MakeActions m
  -> [ExternsFile]
  -> Module
  -> m ExternsFile
rebuildModule actions externs m = do
  env <- fmap fst . runWriterT $ foldM externsEnv primEnv externs
  rebuildModule' actions env externs m

rebuildModule'
  :: forall m
   . (MonadBaseControl IO m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => MakeActions m
  -> Env
  -> [ExternsFile]
  -> Module
  -> m ExternsFile
rebuildModule' act env ext mdl = rebuildModuleWithIndex act env ext mdl Nothing

rebuildModuleWithIndex
  :: forall m
   . (MonadBaseControl IO m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => MakeActions m
  -> Env
  -> [ExternsFile]
  -> Module
  -> Maybe (Int, Int)
  -> m ExternsFile
rebuildModuleWithIndex MakeActions{..} exEnv externs m@(Module _ _ moduleName _ _) moduleIndex = do
  !_ <- pure $ unsafePerformIO $ putStrLn ("### rebuildModuleWithIndex (" <> show moduleName <> ") pre")
  progress $ CompilingModule moduleName moduleIndex
  let env = foldl' (flip applyExternsFileToEnvironment) initEnvironment externs
      withPrim = importPrim m
  lint withPrim
  !_ <- pure $ unsafePerformIO $ putStrLn ("### rebuildModuleWithIndex (" <> show moduleName <> ") linted")

  ((Module ss coms _ elaborated exps, env'), nextVar) <- runSupplyT 0 $ do
    (desugared, (exEnv', usedImports)) <- runStateT (desugar externs withPrim) (exEnv, mempty)
    !_ <- pure $ unsafePerformIO $ putStrLn ("### rebuildModuleWithIndex (" <> show moduleName <> ") desugared")
    let modulesExports = (\(_, _, exports) -> exports) <$> exEnv'
    (checked, CheckState{..}) <- runStateT (typeCheckModule modulesExports desugared) $ emptyCheckState env
    !_ <- pure $ unsafePerformIO $ putStrLn ("### rebuildModuleWithIndex (" <> show moduleName <> ") type checked")
    let usedImports' = foldl' (flip $ \(fromModuleName, newtypeCtorName) ->
          M.alter (Just . (fmap DctorName newtypeCtorName :) . fold) fromModuleName) usedImports checkConstructorImportsForCoercible
    -- Imports cannot be linted before type checking because we need to
    -- known which newtype constructors are used to solve Coercible
    -- constraints in order to not report them as unused.
    censor (addHint (ErrorInModule moduleName)) $ lintImports checked exEnv' usedImports'
    !_ <- pure $ unsafePerformIO $ putStrLn ("### rebuildModuleWithIndex (" <> show moduleName <> ") linted imports")
    return (checked, checkEnv)

  -- desugar case declarations *after* type- and exhaustiveness checking
  -- since pattern guards introduces cases which the exhaustiveness checker
  -- reports as not-exhaustive.
  (deguarded, nextVar') <- runSupplyT nextVar $ do
    desugarCaseGuards elaborated
  !_ <- pure $ unsafePerformIO $ putStrLn ("### rebuildModuleWithIndex (" <> show moduleName <> ") desugared case")

  let upstreamDBs = M.fromList $ (\e -> (efModuleName e, efOurCacheShapes e)) <$> externs

  regrouped <- createBindingGroups moduleName . collapseBindingGroups $ deguarded
  !_ <- pure $ unsafePerformIO $ putStrLn ("### rebuildModuleWithIndex (" <> show moduleName <> ") created binding groups")
  let mod' = Module ss coms moduleName regrouped exps
      corefn = CF.moduleToCoreFn env' mod'
      (optimized, nextVar'') = runSupply nextVar' $ CF.optimizeCoreFn corefn
      (renamedIdents, renamed) = renameInModule optimized
      exts = moduleToExternsFile upstreamDBs mod' env' renamedIdents
  ffiCodegen renamed
  !_ <- pure $ unsafePerformIO $ putStrLn ("### rebuildModuleWithIndex (" <> show moduleName <> ") ffi codegen done")

  -- It may seem more obvious to write `docs <- Docs.convertModule m env' here,
  -- but I have not done so for two reasons:
  -- 1. This should never fail; any genuine errors in the code should have been
  -- caught earlier in this function. Therefore if we do fail here it indicates
  -- a bug in the compiler, which should be reported as such.
  -- 2. We do not want to perform any extra work generating docs unless the
  -- user has asked for docs to be generated.
  let docs = case Docs.convertModule externs exEnv env' m of
               Left errs -> internalError $
                 "Failed to produce docs for " ++ T.unpack (runModuleName moduleName)
                 ++ "; details:\n" ++ prettyPrintMultipleErrors defaultPPEOptions errs
               Right d -> d

  evalSupplyT nextVar'' $ codegen env renamed docs exts
  !_ <- pure $ unsafePerformIO $ putStrLn ("### rebuildModuleWithIndex (" <> show moduleName <> ") purs codegen done")
  return exts

-- | Compiles in "make" mode, compiling each module separately to a @.js@ file and an @externs.cbor@ file.
--
-- If timestamps or hashes have not changed, existing externs files can be used to provide upstream modules' types without
-- having to typecheck those modules again.
make :: forall m. (MonadBaseControl IO m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
     => MakeActions m
     -> [CST.PartialResult Module]
     -> m [ExternsFile]
make ma@MakeActions{..} ms = do
  !_ <- pure $ unsafePerformIO $ putStrLn ("### pre check module names")
  checkModuleNames
  !_ <- pure $ unsafePerformIO $ putStrLn ("### check module names done")
  cacheDb <- readCacheDb
  !_ <- pure $ unsafePerformIO $ putStrLn ("### readCacheDB done")

  (sorted, graph) <- sortModules Transitive (moduleSignature . CST.resPartial) ms
  !_ <- pure $ unsafePerformIO $ putStrLn ("### sortModules done")

  (buildPlan, newCacheDb) <- BuildPlan.construct ma cacheDb (sorted, graph)
  !_ <- pure $ unsafePerformIO $ putStrLn ("### BuildPlan.construct done")

  let toBeRebuilt = filter (BuildPlan.needsRebuild buildPlan . getModuleName . CST.resPartial) sorted
  let totalModuleCount = length toBeRebuilt
  for_ toBeRebuilt $ \m -> fork $ do
    let moduleName = getModuleName . CST.resPartial $ m
    let deps = fromMaybe (internalError "make: module not found in dependency graph.") (lookup moduleName graph)
    buildModule buildPlan moduleName totalModuleCount
      (spanName . getModuleSourceSpan . CST.resPartial $ m)
      (fst $ CST.resFull m)
      (fmap importPrim . snd $ CST.resFull m)
      (deps `inOrderOf` map (getModuleName . CST.resPartial) sorted)

      -- Prevent hanging on other modules when there is an internal error
      -- (the exception is thrown, but other threads waiting on MVars are released)
      `onExceptionLifted` BuildPlan.markComplete buildPlan moduleName (BuildJobFailed mempty)
  !_ <- pure $ unsafePerformIO $ putStrLn ("### BuildPlan fork loop done")

  -- Wait for all threads to complete, and collect results (and errors).
  (failures, successes) <-
    let
      splitResults = \case
        BuildJobSucceeded _ exts ->
          Right exts
        BuildJobFailed errs ->
          Left errs
        BuildJobSkipped ->
          Left mempty
    in
      M.mapEither splitResults <$> BuildPlan.collectResults buildPlan

  !_ <- pure $ unsafePerformIO $ putStrLn ("### BuildPlan collectResults done")
  -- Write the updated build cache database to disk
  writeCacheDb $ Cache.removeModules (M.keysSet failures) newCacheDb
  !_ <- pure $ unsafePerformIO $ putStrLn ("### writeCacheDb done")

  writePackageJson
  !_ <- pure $ unsafePerformIO $ putStrLn ("### writePackageJson done")

  -- If generating docs, also generate them for the Prim modules
  outputPrimDocs
  !_ <- pure $ unsafePerformIO $ putStrLn ("### outputPrimDocs done")

  -- All threads have completed, rethrow any caught errors.
  let errors = M.elems failures
  unless (null errors) $ throwError (mconcat errors)

  -- Here we return all the ExternsFile in the ordering of the topological sort,
  -- so they can be folded into an Environment. This result is used in the tests
  -- and in PSCI.
  let lookupResult mn =
        fromMaybe (internalError "make: module not found in results")
        $ M.lookup mn successes
  return (map (lookupResult . getModuleName . CST.resPartial) sorted)

  where
  checkModuleNames :: m ()
  checkModuleNames = checkNoPrim *> checkModuleNamesAreUnique

  checkNoPrim :: m ()
  checkNoPrim =
    for_ ms $ \m ->
      let mn = getModuleName $ CST.resPartial m
      in when (isBuiltinModuleName mn) $
           throwError
             . errorMessage' (getModuleSourceSpan $ CST.resPartial m)
             $ CannotDefinePrimModules mn

  checkModuleNamesAreUnique :: m ()
  checkModuleNamesAreUnique =
    for_ (findDuplicates (getModuleName . CST.resPartial) ms) $ \mss ->
      throwError . flip foldMap mss $ \ms' ->
        let mn = getModuleName . CST.resPartial . NEL.head $ ms'
        in errorMessage'' (fmap (getModuleSourceSpan . CST.resPartial) ms') $ DuplicateModule mn

  -- Find all groups of duplicate values in a list based on a projection.
  findDuplicates :: Ord b => (a -> b) -> [a] -> Maybe [NEL.NonEmpty a]
  findDuplicates f xs =
    case filter ((> 1) . length) . NEL.groupBy ((==) `on` f) . sortOn f $ xs of
      [] -> Nothing
      xss -> Just xss

  -- Sort a list so its elements appear in the same order as in another list.
  inOrderOf :: (Ord a) => [a] -> [a] -> [a]
  inOrderOf xs ys = let s = S.fromList xs in filter (`S.member` s) ys

  buildModule :: BuildPlan -> ModuleName -> Int -> FilePath -> [CST.ParserWarning] -> Either (NEL.NonEmpty CST.ParserError) Module -> [ModuleName] -> m ()
  buildModule buildPlan moduleName cnt fp pwarnings mres deps = do
    !_ <- pure $ unsafePerformIO $ putStrLn ("### buildModule (" <> show moduleName <> ") pre")
    result <- flip catchError (return . BuildJobFailed) $ do
      let pwarnings' = CST.toMultipleWarnings fp pwarnings
      tell pwarnings'
      m <- CST.unwrapParserError fp mres
      !_ <- pure $ unsafePerformIO $ putStrLn ("### buildModule (" <> show moduleName <> ") unwrapped parser errors")
      -- We need to wait for dependencies to be built, before checking if the current
      -- module should be rebuilt, so the first thing to do is to wait on the
      -- MVars for the module's dependencies.
      mexterns <- fmap unzip . sequence <$> traverse (getResult buildPlan) deps
      !_ <- pure $ unsafePerformIO $ putStrLn ("### buildModule (" <> show moduleName <> ") getResult buildPlan deps; done waiting for deps to build so we can load their externs")
      -- TODO[drathier]: deps is very large; is it all transitive deps? does it differ from mainline?
      case mexterns of
        Just (_, externs) -> do
          -- We need to ensure that all dependencies have been included in Env
          C.modifyMVar_ (bpEnv buildPlan) $ \env -> do
            let
              go :: Env -> ModuleName -> m Env
              go e dep = case lookup dep (zip deps externs) of -- TODO[drathier]: linear search through a linked list here?
                Just exts
                  | not (M.member dep e) -> externsEnv e exts
                _ -> return e
            res <- foldM go env deps
            !_ <- pure $ unsafePerformIO $ putStrLn ("### buildModule (" <> show moduleName <> ") env size: " <> show (M.size res))
            pure res
          !_ <- pure $ unsafePerformIO $ putStrLn ("### buildModule (" <> show moduleName <> ") added all modules to our Env")
          env <- C.readMVar (bpEnv buildPlan)
          idx <- C.takeMVar (bpIndex buildPlan)
          C.putMVar (bpIndex buildPlan) (idx + 1)
          !_ <- pure $ unsafePerformIO $ putStrLn ("### buildModule (" <> show moduleName <> ") added ourselves to buildplan")
          let cfa = getCacheFilesAvailable buildPlan moduleName
          nothingIfNeedsRecompileBecauseOutputFileIsMissing <- touchOutputTimestamp moduleName
          !_ <- pure $ unsafePerformIO $ putStrLn ("### buildModule (" <> show moduleName <> ") touched output timestamp")

          let doCompile wasCacheHit badExts =
                do
                  (exts, warnings) <- listen $ rebuildModuleWithIndex ma env externs m (Just (idx, cnt))
                  let meta = (("cfa" :: String, cfa), ("badExts" :: String, badExts), ("exts" :: String, exts))
                  case (badExts, wasCacheHit) of
                    (Just e, WasCacheMiss) | e == exts ->
                      trace (show moduleName <> ": ⚠️ BUG_PLEASE_REPORT ⚠️ https://github.com/drathier/purescript/issues pointless rebuild, should have been a cache hit. Context, for debugging: " <> sShow meta <> "\n⚠️ BUG_PLEASE_REPORT ⚠️ https://github.com/drathier/purescript/issues \n") (pure ())
                    (Just e, WasCacheHit) | e /= exts ->
                      trace (show moduleName <> ": ⚠️ BUG_PLEASE_REPORT ⚠️ https://github.com/drathier/purescript/issues missing rebuild, caching system said it was a cache hit but rebuilding it changed some files. Context, for debugging: " <> sShow meta <> "\n⚠️ BUG_PLEASE_REPORT ⚠️ https://github.com/drathier/purescript/issues \n") (pure ())
                    _ -> pure ()
                  return $ BuildJobSucceeded (pwarnings' <> warnings) exts

          -- [drathier]: so that we can quickly go back and forth between caching and non-caching versions when testing this out
          experimentalCachingDisabledViaEnvvar <- do
            v <- pure $ unsafePerformIO $ lookupEnv "PURS_DISABLE_EXPERIMENTAL_CACHE"
            pure $ case v of
              Just "0" -> False
              Just "no" -> False
              Just "false" -> False
              Just "False" -> False
              Just "FALSE" -> False
              Just "" -> False
              Nothing -> False
              _ -> True

          let x = shouldRecompile moduleName cfa externs
          !_ <- pure $ unsafePerformIO $ putStrLn ("### buildModule (" <> show moduleName <> ") needs rebuild: " <> (case x of
              Right _ -> "CacheHit"
              Left Nothing -> "CacheMiss-NeverBuilt"
              Left (Just _) -> "CacheMiss-NeedsRebuild"
            ))
          case x of
            Right badExts | experimentalCachingDisabledViaEnvvar -> doCompile WasCacheHit (Just badExts)
            Left badExts | experimentalCachingDisabledViaEnvvar -> doCompile WasCacheMiss badExts
            --
            Right exts
              -- touch the already up-to-date output files so that the next compile run thinks that they're up to date, or recompile if anything was missing
              | Just () <- nothingIfNeedsRecompileBecauseOutputFileIsMissing
              ->
              return $ BuildJobSucceeded pwarnings' exts
            Right badExts -> doCompile WasCacheHit (Just badExts)
            Left badExts -> doCompile WasCacheMiss badExts
        Nothing -> return BuildJobSkipped

    !_ <- pure $ unsafePerformIO $ putStrLn ("### buildModule (" <> show moduleName <> ") done build")
    BuildPlan.markComplete buildPlan moduleName result
    !_ <- pure $ unsafePerformIO $ putStrLn ("### buildModule (" <> show moduleName <> ") marked build complete")
    pure ()

  onExceptionLifted :: m a -> m b -> m a
  onExceptionLifted l r = control $ \runInIO -> runInIO l `onException` runInIO r

data WasCacheHit = WasCacheHit | WasCacheMiss

-- | Infer the module name for a module by looking for the same filename with
-- a .js extension.
inferForeignModules
  :: forall m
   . MonadIO m
  => M.Map ModuleName (Either RebuildPolicy FilePath)
  -> m (M.Map ModuleName FilePath)
inferForeignModules =
    fmap (M.mapMaybe id) . traverse inferForeignModule
  where
    inferForeignModule :: Either RebuildPolicy FilePath -> m (Maybe FilePath)
    inferForeignModule (Left _) = return Nothing
    inferForeignModule (Right path) = do
       jsForeign <- do
            let jsFile = replaceExtension path "js"
            exists <- liftIO $ doesFileExist jsFile
            if exists
              then return (Just jsFile)
              else return Nothing
       erlForeign <- Erl.Build.inferForeignModule' path
       pure (erlForeign <|> jsForeign)

