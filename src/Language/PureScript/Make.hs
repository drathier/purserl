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

import Prelude

import Control.Concurrent.Lifted as C
import Control.DeepSeq (force)
import Control.Exception.Lifted (onException, bracket_, evaluate)
import Control.Monad (foldM, unless, when, (<=<))
import Control.Monad.Base (MonadBase(liftBase))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Supply (evalSupplyT, runSupply, runSupplyT)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Trans.State (runStateT)
import Control.Monad.Writer.Class (MonadWriter(..), censor)
import Control.Monad.Writer.Strict (runWriterT, lift)
import Data.Function (on)
import Data.Foldable (fold, for_)
import Data.List (foldl', sortOn)
import Data.List.NonEmpty qualified as NEL
import Data.Maybe (fromMaybe)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Debug.Trace (traceMarkerIO)
import Language.PureScript.AST (ErrorMessageHint(..), Module(..), SourceSpan(..), getModuleName, getModuleSourceSpan, importPrim)
import Language.PureScript.Crash (internalError)
import Language.PureScript.CST qualified as CST
import Language.PureScript.Docs.Convert qualified as Docs
import Language.PureScript.Environment (initEnvironment)
import Language.PureScript.Errors (MultipleErrors(..), SimpleErrorMessage(..), addHint, defaultPPEOptions, errorMessage', errorMessage'', prettyPrintMultipleErrors)
-- import Language.PureScript.Externs (ExternsFile, applyExternsFileToEnvironment, moduleToExternsFile)
import Language.PureScript.Externs
import Language.PureScript.Linter (Name(..), lint, lintImports)
import Language.PureScript.ModuleDependencies (DependencyDepth(..), moduleSignature, sortModules)
import Language.PureScript.Names (ModuleName, isBuiltinModuleName, runModuleName)
import Language.PureScript.Renamer (renameInModule)
import Language.PureScript.Sugar (Env, collapseBindingGroups, createBindingGroups, desugar, desugarCaseGuards, externsEnv, primEnv)
import Language.PureScript.TypeChecker (CheckState(..), emptyCheckState, typeCheckModule)
import Language.PureScript.Make.BuildPlan (BuildJobResult(..), BuildPlan(..))
import Language.PureScript.Make.BuildPlan qualified as BuildPlan
import Language.PureScript.Make.Cache qualified as Cache
import Language.PureScript.Make.Actions as Actions
import Language.PureScript.Make.Monad as Monad
import Language.PureScript.CoreFn qualified as CF
import System.Directory (doesFileExist)
import System.FilePath (replaceExtension)
import System.Environment (lookupEnv)
import Debug.Trace
import System.IO.Unsafe (unsafePerformIO)
import PrettyPrint
import Data.Text qualified as T
import Data.Text.IO qualified as T

-- purserl
import Control.Applicative ((<|>))
import qualified Build as Erl.Build
import           System.Directory (getCurrentDirectory)
-- import System.IO.Unsafe (unsafePerformIO)
--


-- | Rebuild a single module.
--
-- This function is used for fast-rebuild workflows (PSCi and psc-ide are examples).
rebuildModule
  :: forall m
   . (MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => MakeActions m
  -> [ExternsFile]
  -> Module
  -> m ExternsFile
rebuildModule actions externs m = do
  env <- fmap fst . runWriterT $ foldM externsEnv primEnv externs
  rebuildModule' actions env externs m

rebuildModule'
  :: forall m
   . (MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => MakeActions m
  -> Env
  -> [ExternsFile]
  -> Module
  -> m ExternsFile
rebuildModule' act env ext mdl = rebuildModuleWithIndex act env ext mdl Nothing UnknownRecompileReason (\_ _ _ -> pure ())

rebuildModuleWithIndex
  :: forall m
   . (MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => MakeActions m
  -> Env
  -> [ExternsFile]
  -> Module
  -> Maybe (Int, Int)
  -> RecompileReason
  -> (ModuleName -> Maybe ExternsFile -> BuildJobResult -> m ())
  -> m ExternsFile
rebuildModuleWithIndex MakeActions{..} exEnv externs m@(Module _ _ moduleName _ _) moduleIndex causedByModule mmarkCompleteWithoutErrors = do
  progress $ CompilingModule moduleName moduleIndex causedByModule
  progress $ CompileMeta ("### CS.goBuildEnv13[" <> runModuleName moduleName <> "]")
  let env = foldl' (flip applyExternsFileToEnvironment) initEnvironment externs
      withPrim = importPrim m
  lint withPrim

  progress $ CompileMeta ("### CS.goDesugar1[" <> runModuleName moduleName <> "]")
  ((checked@(Module ss coms _ elaborated exps), env', exEnv', usedImports'), nextVar) <- runSupplyT 0 $ do
    -- lift $ progress $ CompilingModule moduleName moduleIndex "2"
    (desugared, (exEnv', usedImports)) <- runStateT (desugar externs withPrim) (exEnv, mempty)
    lift $ progress $ CompileMeta ("### CS.goTypeCheck2[" <> runModuleName moduleName <> "]")
    -- lift $ progress $ CompilingModule moduleName moduleIndex "3"
    let modulesExports = (\(_, _, exports) -> exports) <$> exEnv'
    -- lift $ progress $ CompilingModule moduleName moduleIndex "4"
    (checked, CheckState{..}) <- runStateT (typeCheckModule modulesExports desugared) $ emptyCheckState env

    let usedImports' = foldl' (flip $ \(fromModuleName, newtypeCtorName) ->
          M.alter (Just . (fmap DctorName newtypeCtorName :) . fold) fromModuleName) usedImports checkConstructorImportsForCoercible

    lift $ progress $ CompileMeta ("### CS.goDesugarCaseGuards4[" <> runModuleName moduleName <> "]")
    return (checked, checkEnv, exEnv', usedImports')


  progress $ CompileMeta ("### CS.goLintImports3[" <> runModuleName moduleName <> "]")
  -- lift $ progress $ CompilingModule moduleName moduleIndex "5"
  -- Imports cannot be linted before type checking because we need to
  -- known which newtype constructors are used to solve Coercible
  -- constraints in order to not report them as unused.
  censor (addHint (ErrorInModule moduleName)) $ lintImports checked exEnv' usedImports'

  -- progress $ CompilingModule moduleName moduleIndex "6"

  -- desugar case declarations *after* type- and exhaustiveness checking
  -- since pattern guards introduces cases which the exhaustiveness checker
  -- reports as not-exhaustive.
  (deguarded, nextVar') <- runSupplyT nextVar $ do
    desugarCaseGuards elaborated
  progress $ CompileMeta ("### CS.goCreateBindingGroups5[" <> runModuleName moduleName <> "]")

  regrouped <- createBindingGroups moduleName . collapseBindingGroups $ deguarded

  progress $ CompileMeta ("### CS.goFfiCodegen6[" <> runModuleName moduleName <> "]")
  let upstreamDBs = M.fromList $ (\e -> (efModuleName e, efOurCacheShapes e)) <$> externs
  let mod' = Module ss coms moduleName regrouped exps
      corefn = CF.moduleToCoreFn env' mod'
      (optimized, nextVar'') = runSupply nextVar' $ CF.optimizeCoreFn corefn
      (renamedIdents, renamed) = renameInModule optimized
      exts = moduleToExternsFile upstreamDBs mod' env' renamedIdents

  mmarkCompleteWithoutErrors moduleName Nothing (BuildJobSucceeded (MultipleErrors []) exts)

  ffiCodegen renamed

  progress $ CompileMeta ("### CS.goCodegen7[" <> runModuleName moduleName <> "]")
  -- progress $ CompilingModule moduleName moduleIndex "7"
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

  -- NOTE[drathier]: codegen updates the ExternsFile cache, so we have to grab a copy of the old externs file before running codegen if we want to diff them
  -- progress $ CompilingModule moduleName moduleIndex "8"
  evalSupplyT nextVar'' $ codegen env renamed docs exts
  -- progress $ CompilingModule moduleName moduleIndex "9"
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
  progress $ CompileMeta ("### CS.goReadCacheDb8")

  checkModuleNames
  cacheDb <- readCacheDb
  progress $ CompileMeta ("### CS.goSortModules9")

  -- let !_ = unsafePerformIO $ putStrLn (show ("cacheDb", cacheDb))

  (sorted, graph) <- sortModules Transitive (moduleSignature . CST.resPartial) ms
  progress $ CompileMeta ("### CS.goConstructBuildPlan10")

  -- (buildPlan2, newCacheDb2) <- BuildPlan.construct2 ma cacheDb (sortedDirect, graphDirect)
  (buildPlan, newCacheDb) <- BuildPlan.construct2 ma cacheDb (sorted, graph)
  progress $ CompileMeta ("### CS.goFork11")

  -- Limit concurrent module builds to the number of capabilities as
  -- (by default) inferred from `+RTS -N -RTS` or set explicitly like `-N4`.
  -- This is to ensure that modules complete fully before moving on, to avoid
  -- holding excess memory during compilation from modules that were paused
  -- by the Haskell runtime.
  capabilities <- getNumCapabilities
  let concurrency = max 1 capabilities
  lock <- C.newQSem concurrency

  let toBeRebuilt = filter (BuildPlan.needsRebuild buildPlan . getModuleName . CST.resPartial) sorted
  progress $ CompileMeta ("### CS.toBeRebuilt")
  let totalModuleCount = length toBeRebuilt
  newCacheDbMVar <- newMVar cacheDb

  for_ toBeRebuilt $ \m -> fork $ do
    -- for each module:
    -- do I need to rebuild myself?
    -- did any of my deps change?
    -- did my source files change?
    -- after recompile, did my externs change?



    let moduleName = getModuleName . CST.resPartial $ m
    -- progress $ CompileMeta (T.pack $ show (moduleName, "-- DR.1"))
    -- for each module:
    -- do I need to rebuild myself?
    -- did my source files change?
    inputInfo <- getInputTimestampsAndHashes moduleName
    areMyOwnFilesUpToDate <-
      case inputInfo of
        Left RebuildNever -> do
          -- built-in module, nothing to do
          -- progress $ CompileMeta (T.pack $ show (moduleName, "-- DR.2.1", "Left RebuildNever"))
          pure True
        Left RebuildAlways -> do
          -- file not yet built
          -- progress $ CompileMeta (T.pack $ show (moduleName, "-- DR.2.2", "Left RebuildAlways"))
          pure False
        Right cacheInfo -> do
          -- -- progress $ CompileMeta (T.pack $ show (moduleName, -- DR.2.3", "Right cacheInfo"))
          -- file has been built before
          cwd <- liftBase getCurrentDirectory
          (newCacheInfo, isUpToDate) <- Cache.checkChanged cacheDb moduleName cwd cacheInfo
          modifyMVar_ newCacheDbMVar (\db -> pure $ M.insert moduleName newCacheInfo db)
          -- -- progress $ CompileMeta (T.pack $ show (moduleName, "-- DR.4.2.1", ("writeNewCacheDb", newCacheInfo)))
          pure isUpToDate

    let bumpCompilationCounter = do
          idx <- C.takeMVar (bpIndex buildPlan)
          C.putMVar (bpIndex buildPlan) (idx + 1)


    let deps = fromMaybe (internalError "make: module not found in dependency graph.") (lookup moduleName graph)
    let goBuild oldExterns results recompileReason = do
          -- progress $ CompileMeta (T.pack $ show (moduleName, "-- DR.4.3"))
          buildModule lock buildPlan moduleName totalModuleCount oldExterns results recompileReason
            (T.unpack . spanName . getModuleSourceSpan . CST.resPartial $ m)
            (fst $ CST.resFull m)
            (fmap importPrim . snd $ CST.resFull m)
            (deps `inOrderOf` map (getModuleName . CST.resPartial) sorted)

            -- Prevent hanging on other modules when there is an internal error
            -- (the exception is thrown, but other threads waiting on MVars are released)
          `onException` (BuildPlan.markComplete ma buildPlan moduleName Nothing (BuildJobFailed mempty))

    case areMyOwnFilesUpToDate of
      False -> do
        -- progress $ CompileMeta (T.pack $ show (moduleName, "-- DR.3.1", ("areMyOwnFilesUpToDate", areMyOwnFilesUpToDate)))
        oldExterns <- BuildPlan.getExternFromLastSuccessfulPreviousBuild ma buildPlan moduleName
        mResults <- BuildPlan.fetchMissingExterns ("mod", "b", moduleName) ma buildPlan deps
        case assertAllExternsExists mResults of
          Left bjRes -> bumpCompilationCounter >> BuildPlan.markComplete ma buildPlan moduleName Nothing bjRes
          Right results -> goBuild oldExterns results SourceChangedOrDependencyFailedToBuildInPreviousCompilationOrSomethingElse
      True -> do
        -- -- progress $ CompileMeta (T.pack $ show (moduleName, -- DR.3.2", ("areMyOwnFilesUpToDate", areMyOwnFilesUpToDate)))
        -- did any of my deps change?
        anyDepChanged <- BuildPlan.anyDepChanged moduleName graph buildPlan
        case anyDepChanged of
          BuildPlan.FailRebuildDepsFailed causedByModule -> do
            -- progress $ CompileMeta (T.pack $ show (moduleName, "-- DR.3.2.1", ("areMyOwnFilesUpToDate", areMyOwnFilesUpToDate), "FailRebuildDepsFailed", causedByModule))
            bumpCompilationCounter
            BuildPlan.markComplete ma buildPlan moduleName Nothing BuildJobSkipped
            pure ()

          BuildPlan.FullDepsCacheHit -> do
            -- -- progress $ CompileMeta (T.pack $ show (moduleName, -- DR.3.2.2", ("areMyOwnFilesUpToDate", areMyOwnFilesUpToDate), "FullDepsCacheHit"))
            bumpCompilationCounter
            BuildPlan.markComplete ma buildPlan moduleName Nothing BuildJobSkippedFullCacheHit
            pure ()

          BuildPlan.DepsChangedPleaseRebuildIfNeeded causedByModule -> do
            -- progress $ CompileMeta (T.pack $ show (moduleName, "-- DR.3.2.3", ("areMyOwnFilesUpToDate", areMyOwnFilesUpToDate), "DepsChangedPleaseRebuildIfNeeded", causedByModule))
            oldExterns <- BuildPlan.getExternFromLastSuccessfulPreviousBuild ma buildPlan moduleName
            mResults <- BuildPlan.fetchMissingExterns ("mod", "a", moduleName) ma buildPlan deps
            case assertAllExternsExists mResults of
              Left bjRes -> bumpCompilationCounter >> BuildPlan.markComplete ma buildPlan moduleName Nothing bjRes
              Right results ->
                case BuildPlan.needsRebuildEvenAfterDiffingCacheShapes oldExterns (fmap snd results) of
                  BuildPlan.NoRebuildNeeded -> do
                    -- progress $ CompileMeta ("-- DR 3.2.3.1 upstreamDiffWasEmpty[" <> runModuleName moduleName <> "]")
                    bumpCompilationCounter
                    BuildPlan.markComplete ma buildPlan moduleName Nothing BuildJobSkippedFullCacheHit
                    pure ()
                  BuildPlan.PleaseRebuild errs -> do
                    -- progress $ CompileMeta ("-- DR 3.2.3.2 upstreamDiffFound[" <> runModuleName moduleName <> "] " <> T.pack (show errs))
                    goBuild oldExterns results (DependencyChanged causedByModule)

  -- progress $ CompileMeta (T.pack $ show ("-- DR.5", "all solo modules done, pre collection"))
  externs <- traverse tryReadMVar $ M.elems $ BuildPlan.bpExterns buildPlan
  -- progress $ CompileMeta (T.pack $ show ("-- DR.5", "all solo modules done, counts", length (filter ((==) Nothing) externs), "/", length externs, "unchanged"))
  -- Wait for all threads to complete, and collect results (and errors).

  collectedResults <- BuildPlan.collectResults buildPlan
  let directFailures =
        let
          isDirectFailure = \case
            BuildJobFailed _ -> True
            BuildJobSucceeded _ _ -> False
            BuildJobSkipped -> False
            BuildJobSkippedFullCacheHit -> False
        in
        M.filter isDirectFailure $ collectedResults

  let (failures, successes) =
        let
          splitResults = \case
            BuildJobSucceeded _ exts ->
              Right exts
            BuildJobFailed errs ->
              Left errs
            BuildJobSkipped ->
              Left mempty
            BuildJobSkippedFullCacheHit ->
              Right (error "FullCacheHit externs not here")
        in
          M.mapEither splitResults $ collectedResults

  progress $ CompileMeta ("### CS.collectedResults31")
  -- Write the updated build cache database to disk
  -- NOTE[drathier]: Leaving the old cache-file as-is on failed compiles is a workaround. Previously, a build error in a module caused the cache entries for all subsequent modules to be dropped, which lead to a recompile. This way, we pretend we never did that failing compile, and we'll recompile modules over and over again until we get a full successful compile. This might play badly with ide and possibly other things too, but it superficially works. It's worth a try.

  newCacheDb <- takeMVar newCacheDbMVar
  writeCacheDb $ Cache.removeModules (M.keysSet directFailures) $ newCacheDb
  progress $ CompileMeta ("### CS.wroteCacheDB32")
  -- case () of
  --   _ | M.null failures == False ->
  --     -- NOTE[drathier]: Leaving the old cache-file as-is on failed compiles is a workaround. Previously, a build error in a module caused the cache entries for all subsequent modules to be dropped, which lead to a recompile. This way, we pretend we never did that failing compile, and we'll recompile modules over and over again until we get a full successful compile. This might play badly with ide and possibly other things too, but it superficially works. It's worth a try.
  --     pure ()
  --   _ ->
  --     writeCacheDb $ Cache.removeModules (M.keysSet failures) newCacheDb

-- caching verkar okej iom removeModules på new successful builds, men vi skriver alldeles för många filer till disk nu. Undvik att toucha och skriva över filer om innehållet ej ändrats, istället för att toucha filer för att få gamla prebuilt-logiken att funka. Ingenting räknas som prebuilt med den här logiken nu. 2023-12-24


  writePackageJson

  -- If generating docs, also generate them for the Prim modules
  outputPrimDocs

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

  buildModule :: QSem -> BuildPlan -> ModuleName -> Int -> Maybe ExternsFile -> M.Map ModuleName (MultipleErrors, ExternsFile) -> RecompileReason -> FilePath -> [CST.ParserWarning] -> Either (NEL.NonEmpty CST.ParserError) Module -> [ModuleName] -> m ()
  buildModule lock buildPlan moduleName cnt oldExts results recompileReason fp pwarnings mres deps = do
    progress $ CompileMeta ("### CS.goParse12[" <> runModuleName moduleName <> "]")

    -- NOTE[drathier]: catchError here only ever fires if there's an error in a module we're building; it does not fire if a module is skipped because upstream modules failed to build.
    result <- flip catchError (return . BuildJobFailed) $ do
      let pwarnings' = CST.toMultipleWarnings fp pwarnings
      tell pwarnings'
      m <- CST.unwrapParserError fp mres
      -- We need to wait for dependencies to be built, before checking if the current
      -- module should be rebuilt, so the first thing to do is to wait on the
      -- MVars for the module's dependencies.

      do
          -- We need to ensure that all dependencies have been included in Env
          C.modifyMVar_ (bpEnv buildPlan) $ \env -> do
            let
              go :: Env -> ModuleName -> m Env
              go e dep = case M.lookup dep results of
                Just (_, exts)
                  | not (M.member dep e) -> externsEnv e exts
                _ -> return e
            foldM go env deps
          env <- C.readMVar (bpEnv buildPlan)
          idx <- C.takeMVar (bpIndex buildPlan)
          C.putMVar (bpIndex buildPlan) (idx + 1)
          -- nothingIfNeedsRecompileBecauseOutputFileIsMissing <- touchOutputTimestamp moduleName

          let doCompile wasCacheHit badExts =
                do
                  -- Bracket all of the per-module work behind the semaphore, including
                  -- forcing the result. This is done to limit concurrency and keep
                  -- memory usage down; see comments above.
                  (exts, warnings) <- bracket_ (C.waitQSem lock) (C.signalQSem lock) $ do
                      -- Eventlog markers for profiling; see debug/eventlog.js
                      liftBase $ traceMarkerIO $ T.unpack (runModuleName moduleName) <> " start"
                      -- liftBase $ traceM $ T.unpack (runModuleName moduleName) <> " start"
                      -- Force the externs and warnings to avoid retaining excess module
                      -- data after the module is finished compiling.
                      extsAndWarnings <- evaluate . force <=< listen $ do
                        rebuildModuleWithIndex ma env (snd <$> M.elems results) m (Just (idx, cnt)) recompileReason (BuildPlan.markComplete ma buildPlan)

                      -- liftBase $ traceM $ T.unpack (runModuleName moduleName) <> " end"
                      liftBase $ traceMarkerIO $ T.unpack (runModuleName moduleName) <> " end"
                      return extsAndWarnings

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

          doCompile WasCacheMiss Nothing
--          case BuildPlan.shouldRecompile buildPlan moduleName externs of
--            Right badExts | experimentalCachingDisabledViaEnvvar -> doCompile WasCacheHit (Just badExts)
--            Left badExts | experimentalCachingDisabledViaEnvvar -> doCompile WasCacheMiss badExts
--            --
--            Right exts
--              -- touch the already up-to-date output files so that the next compile run thinks that they're up to date, or recompile if anything was missing
--              | Just () <- nothingIfNeedsRecompileBecauseOutputFileIsMissing
--              ->
--              return $ BuildJobSucceeded pwarnings' exts
--            Right badExts -> doCompile WasCacheHit (Just badExts)
--            Left badExts -> doCompile WasCacheMiss badExts

    BuildPlan.remarkComplete ma buildPlan moduleName oldExts result


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


assertAllExternsExists :: M.Map ModuleName BuildJobResult -> Either BuildJobResult (M.Map ModuleName (MultipleErrors, ExternsFile))
assertAllExternsExists externs = assertAllExternsExistsImpl (M.toList externs) mempty

assertAllExternsExistsImpl externs acc =
  case externs of
    [] -> Right acc
    ((moduleName,e):ex) ->
      case e of
        BuildJobSkippedFullCacheHit -> internalError "Make:assertAllExternsExists saw unexpected BuildJobSkippedFullCacheHit, should have been replaced with BuildJobSucceeded by now"
        BuildJobFailed err -> Left BuildJobSkipped
        BuildJobSkipped -> Left BuildJobSkipped
        --
        BuildJobSucceeded err ext -> assertAllExternsExistsImpl ex (M.insert moduleName (err, ext) acc)
