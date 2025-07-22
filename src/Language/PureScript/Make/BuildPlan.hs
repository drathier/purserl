module Language.PureScript.Make.BuildPlan
  ( BuildPlan(bpEnv, bpIndex)
  , BuildJobResult(..)
  , bpExterns
  , construct
  , construct2
  , getExternFromLastSuccessfulPreviousBuild
  , needsRebuildEvenAfterDiffingCacheShapes
  , CacheShapeDiffResult(..)
  , anyDepChanged
  , RebuildInstructions(..)
  , getResult
  , fetchMissingExterns
  , collectResults
  , markComplete
  , markComplete2
  , needsRebuild
  ) where

import Prelude

import Codec.Serialise (serialise)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.Async.Lifted as A
import Control.Concurrent.Lifted as C
import Control.Monad.Base (liftBase)
-- import Control.Monad (foldM)
import Control.Monad
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Foldable (foldl')
import Data.Map qualified as M
import Data.Map.Merge.Strict qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Time.Clock (UTCTime)
import Language.PureScript.AST (Module, getModuleName)
import Language.PureScript.Crash (internalError)
import Language.PureScript.CST qualified as CST
import Language.PureScript.Errors (MultipleErrors(..))
-- import Language.PureScript.Externs (ExternsFile)
import Language.PureScript.Externs
import qualified Language.PureScript.Make.Actions as Actions
import Language.PureScript.Make.Actions (MakeActions(..), RebuildPolicy(..), ProgressMessage(..))
import Language.PureScript.Make.Cache (CacheDb, CacheInfo, checkChanged)
import Language.PureScript.Names (ModuleName, runModuleName)
import Language.PureScript.Sugar.Names.Env (Env, primEnv)
import System.Directory (getCurrentDirectory)
import qualified Data.Text as T
import Debug.Trace
import PrettyPrint
import Data.Foldable
import DH qualified

scratchpad = do
  -- did any dep input file hashes change?
  -- if so, did their hashes change?
  -- inputInfo <- getInputTimestampsAndHashes moduleName
  -- cacheChanged <- A.forConcurrently sortedModuleNames getRebuildStatusIsUpToDate
  Just 42

-- | The BuildPlan tracks information about our build progress, and holds all
-- prebuilt modules for incremental builds.
data BuildPlan = BuildPlan
  { bpPrebuilt :: M.Map ModuleName Prebuilt
  , bpDirtyExterns :: M.Map ModuleName CacheFilesAvailable
  , bpBuildJobs :: M.Map ModuleName BuildJob
  , bpEnv :: C.MVar Env
  , bpIndex :: C.MVar Int
  -- todo[drathier]: mvar map of mvars is slow
  , bpCacheResult :: M.Map ModuleName (MVar (Maybe CacheResult))
  , bpExterns :: M.Map ModuleName (MVar (Maybe ExternsFile))
  }

data CacheResult
  = NoExternsChange
  | ExternsChanged
  deriving(Show)

data RebuildInstructions
  = DepsChangedPleaseRebuildIfNeeded ModuleName
  | FailRebuildDepsFailed ModuleName
  | FullDepsCacheHit
  deriving(Show)

data Prebuilt = Prebuilt
  { pbModificationTime :: UTCTime
  , pbExternsFile :: ExternsFile
  }
  deriving (Show)

newtype BuildJob = BuildJob
  { bjResult :: C.MVar BuildJobResult
    -- ^ Note: an empty MVar indicates that the build job has not yet finished.
  }

data BuildJobResult
  = BuildJobSucceeded !MultipleErrors !ExternsFile
  -- ^ Succeeded, with warnings and externs
  --
  | BuildJobFailed !MultipleErrors
  -- ^ Failed, with errors

  | BuildJobSkipped
  -- ^ The build job was not run, because an upstream build job failed

  | BuildJobSkippedFullCacheHit
  -- ^ The build job was not run, because no upstream files changed

-- | Information obtained about a particular module while constructing a build
-- plan; used to decide whether a module needs rebuilding.
data RebuildStatus = RebuildStatus
  { statusModuleName :: ModuleName
  , statusRebuildNever :: Bool
  , statusNewCacheInfo :: Maybe CacheInfo
    -- ^ New cache info for this module which should be stored for subsequent
    -- incremental builds. A value of Nothing indicates that cache info for
    -- this module should not be stored in the build cache, because it is being
    -- rebuilt according to a RebuildPolicy instead.
  , statusPrebuilt :: Maybe Prebuilt
    -- ^ Prebuilt externs and timestamp for this module, if any.
  , statusDirtyExterns :: Maybe ExternsFile
    -- ^ Externs, even if the source file is changed or the timestamp check fails.
  }

-- | Called when we finished compiling a module and want to report back the
-- compilation result, as well as any potential errors that were thrown.
markComplete
  :: (MonadBaseControl IO m)
  => MakeActions m
  -> BuildPlan
  -> ModuleName
  -> Maybe ExternsFile
  -> BuildJobResult
  -> m ()
markComplete ma buildPlan moduleName oldExt result = do
  liftBase $ case result of
      BuildJobSucceeded _ _ ->
        putStrLn $ "### CS.BuildJobSucceeded[" <> T.unpack (runModuleName moduleName) <> "]"
      BuildJobFailed _ ->
        putStrLn $ "### CS.BuildJobFailed[" <> T.unpack (runModuleName moduleName) <> "]"
      BuildJobSkipped ->
        putStrLn $ "### CS.BuildJobSkipped[" <> T.unpack (runModuleName moduleName) <> "]"
      BuildJobSkippedFullCacheHit ->
        -- putStrLn $ "### CS.BuildJobSkippedFullCacheHit[" <> T.unpack (runModuleName moduleName) <> "]"
        pure ()
  let BuildJob rVar = fromMaybe (internalError "make: markComplete no barrier") $ M.lookup moduleName (bpBuildJobs buildPlan)
  DH.hasLocked "1" $ putMVar rVar result

  markComplete2 ma buildPlan moduleName oldExt result


-- | Called when we finished compiling a module and want to report back the
-- compilation result, as well as any potential errors that were thrown.
markComplete2
  :: (MonadBaseControl IO m)
  => MakeActions m
  -> BuildPlan
  -> ModuleName
  -> Maybe ExternsFile
  -> BuildJobResult
  -> m ()
markComplete2 ma@MakeActions{..} buildPlan moduleName oldExt result = do
--  liftBase $ putStrLn $ case result of
--      BuildJobSucceeded _ _ ->
--        "### CS.BuildJobSucceeded[" <> T.unpack (runModuleName moduleName) <> "]"
--      BuildJobFailed _ ->
--        "### CS.BuildJobFailed[" <> T.unpack (runModuleName moduleName) <> "]"
--      BuildJobSkipped ->
--        "### CS.BuildJobSkipped[" <> T.unpack (runModuleName moduleName) <> "]"

  --(_,oldExt) <- fetchMissingExtern () ma buildPlan moduleName
  let cfa = getCacheFilesAvailable buildPlan moduleName
--  (case result of
--    BuildJobFailed _ -> pure ()
--    BuildJobSkipped -> pure ()
--    BuildJobSkippedFullCacheHit -> pure ()
--    BuildJobSucceeded _ newExt -> do
--      progress $ CompileMeta (T.pack $ show ("-- BP.externsDiff[" <> runModuleName moduleName <> "]", ("eq?", Just newExt == oldExt), ("serialise-eq?", Just (serialise newExt) == fmap serialise oldExt), ("serialise-opaque-eq?", serialiseDbEq newExt oldExt)))
--      progress $ CompileMeta (T.pack $ show ("-- BP.externsDiff[" <> runModuleName moduleName <> "]New", Just newExt))
--      progress $ CompileMeta (T.pack $ show ("-- BP.externsDiff[" <> runModuleName moduleName <> "]Old", oldExt))
--
--      pure ()
--    )
  DH.hasLocked "2" $ putMVar
    (fromMaybe (internalError (show ("BuildPlan: bpCacheResult mvar not found for module", moduleName))) $ M.lookup moduleName (bpCacheResult buildPlan))

    (case result of
      BuildJobFailed _ -> Nothing
      BuildJobSkipped -> Nothing
      BuildJobSkippedFullCacheHit -> Just NoExternsChange
      BuildJobSucceeded _ newExt -> do
        case fmap (serialise . efOurCacheShapes) oldExt == Just (serialise $ efOurCacheShapes newExt) of
          True ->
            Just NoExternsChange
          False ->
            Just ExternsChanged
    )

serialiseDbEq (ExternsFile efVersion1 efModuleName1 efExports1 efImports1 efFixities1 efTypeFixities1 efDeclarations1 efSourceSpan1 efUpstreamCacheShapes1 efOurCacheShapes1) mb =
  case mb of
    Nothing -> []
    Just (ExternsFile efVersion2 efModuleName2 efExports2 efImports2 efFixities2 efTypeFixities2 efDeclarations2 efSourceSpan2 efUpstreamCacheShapes2 efOurCacheShapes2) ->
      filter
      (\(x, y) -> y == False)
      [ ("efVersion", serialise efVersion1 == serialise efVersion2)
      , ("efModuleName", serialise efModuleName1 == serialise efModuleName2)
      , ("efExports", serialise efExports1 == serialise efExports2)
      , ("efImports", serialise efImports1 == serialise efImports2)
      , ("efFixities", serialise efFixities1 == serialise efFixities2)
      , ("efTypeFixities", serialise efTypeFixities1 == serialise efTypeFixities2)
      , ("efDeclarations", serialise efDeclarations1 == serialise efDeclarations2)
      , ("efSourceSpan", serialise efSourceSpan1 == serialise efSourceSpan2)
      , ("efUpstreamCacheShapes", serialise efUpstreamCacheShapes1 == serialise efUpstreamCacheShapes2)
      , ("efOurCacheShapes", serialise efOurCacheShapes1 == serialise efOurCacheShapes2)
      ]

-- | Whether or not the module with the given ModuleName needs to be rebuilt
needsRebuild :: BuildPlan -> ModuleName -> Bool
needsRebuild bp moduleName = M.member moduleName (bpBuildJobs bp)

-- | Collects results for all prebuilt as well as rebuilt modules. This will
-- block until all build jobs are finished. Prebuilt modules always return no
-- warnings.
collectResults
  :: (MonadBaseControl IO m)
  => BuildPlan
  -> m (M.Map ModuleName BuildJobResult)
collectResults buildPlan = do
  let prebuiltResults = M.map (BuildJobSucceeded (MultipleErrors []) . pbExternsFile) (bpPrebuilt buildPlan)
  barrierResults <- traverse (DH.hasLocked "3" . readMVar . bjResult) $ bpBuildJobs buildPlan
  pure (M.union prebuiltResults barrierResults)

-- | Gets the the build result for a given module name independent of whether it
-- was rebuilt or prebuilt. Prebuilt modules always return no warnings.
getResult
  :: (MonadBaseControl IO m)
  => BuildPlan
  -> ModuleName
  -> m BuildJobResult
getResult buildPlan moduleName = do
  case M.lookup moduleName (bpPrebuilt buildPlan) of
    Just es ->
      pure (BuildJobSucceeded (MultipleErrors []) (pbExternsFile es))
    Nothing -> do
      DH.hasLocked ("4.getResult", moduleName) $ readMVar $ bjResult $ fromMaybe (internalError "make: no barrier") $ M.lookup moduleName (bpBuildJobs buildPlan)

fetchMissingExtern :: Show meta => MonadBaseControl IO m => meta -> MakeActions m -> BuildPlan -> ModuleName -> m BuildJobResult
fetchMissingExtern meta MakeActions{..} buildPlan moduleName = do
  mExts <- getResult buildPlan moduleName
  case mExts of
    BuildJobSucceeded warns v -> pure mExts
    BuildJobFailed err -> pure mExts
    -- TODO[drathier]: perhaps put BuildJobSkippedFullCacheHit externs into bjResult? Optional externs field?
    BuildJobSkipped -> pure mExts
    BuildJobSkippedFullCacheHit -> do
      let mvar = fromMaybe (internalError "BuildPlan: fetchMissingExtern") $ M.lookup moduleName (bpExterns buildPlan)
      e <- DH.hasLocked "5" $ readMVar mvar
      -- read mvar, it's probably already filled and we don't want to interrupt anyone
      case e of
        -- Maybe wrapped value instead of tryReadMVar so that we don't have two threads decoding the same externs file, wasting work
        Just ext -> pure (BuildJobSucceeded (MultipleErrors []) ext)
        Nothing -> do
          -- oops, better fill in the mvar
          mv <- DH.hasLocked "6" $ takeMVar mvar
          case mv of
            -- nope, someone did it before us
            Just extern -> do
              DH.hasLocked "7" $ putMVar mvar mv
              pure (BuildJobSucceeded (MultipleErrors []) extern)

            Nothing -> do
              -- fill it in
              mextern <- snd <$> readExterns moduleName
              let extern = fromMaybe (internalError (show ("BuildPlan readExterns", moduleName, meta))) mextern
              DH.hasLocked "8" $ putMVar mvar (Just extern)
              pure (BuildJobSucceeded (MultipleErrors []) extern)

fetchMissingExterns :: Show meta => MonadBaseControl IO m => meta -> MakeActions m -> BuildPlan -> [ModuleName] -> m (M.Map ModuleName BuildJobResult)
fetchMissingExterns meta ma buildPlan deps =
  M.fromList <$> traverse (\dep -> (dep,) <$> fetchMissingExtern meta ma buildPlan dep) deps

data CacheShapeDiffResult
  = PleaseRebuild [(ModuleName, DBOpaque)]
  | NoRebuildNeeded

needsRebuildEvenAfterDiffingCacheShapes Nothing upstream = PleaseRebuild []
needsRebuildEvenAfterDiffingCacheShapes (Just oldExts) upstream =
  let ourCachedUpstreamCacheShapes = efUpstreamCacheShapes oldExts in
  let relevantUpstreamModules = M.intersectionWith (\_ s -> efOurCacheShapes s) ourCachedUpstreamCacheShapes $ upstream in
  let moduleName = efModuleName oldExts in

  -- TODO[drathier]: only diff exports list if it's an unsafe import
  let res =
        M.merge
          (M.mapMissing (\k a -> internalError (show ("BuildPlan: an upstream cache shape has disappeared since the last rebuild", k, a))))
          (M.mapMissing (\k b -> [(k,b)]))
          (M.zipWithMatched (\k a b ->
            let x = dbOpaqueDiffDiffIgnoringExportsListChanges a b in
            if x == mempty
            then [] else [(k,x)]
          ))
          ourCachedUpstreamCacheShapes
          relevantUpstreamModules
  in case fold res of
    [] ->
      NoRebuildNeeded
    errs ->
      PleaseRebuild errs

data CacheFilesAvailable
  = DepChanged Prebuilt
  | SourceChanged
  | UpToDate Prebuilt

instance Show CacheFilesAvailable where
  show cfa =
    case cfa of
      UpToDate _ -> "UpToDate.."
      SourceChanged -> "SourceChanged"
      DepChanged _ -> "DepChanged.."

cfaPrebuilt :: CacheFilesAvailable -> Maybe Prebuilt
cfaPrebuilt cfa =
  case cfa of
    DepChanged pb -> Just pb
    SourceChanged -> Nothing
    UpToDate pb -> Just pb

-- | Gets the the build result for a given module name independent of whether it
-- was rebuilt or prebuilt. Prebuilt modules always return no warnings.
getCacheFilesAvailable
  :: BuildPlan
  -> ModuleName
  -> CacheFilesAvailable
getCacheFilesAvailable buildPlan moduleName =
  case M.lookup moduleName (bpDirtyExterns buildPlan) of
    Just v -> v
    Nothing -> SourceChanged

getExternFromLastSuccessfulPreviousBuild :: Monad m => MakeActions m -> BuildPlan -> ModuleName -> m (Maybe ExternsFile)
getExternFromLastSuccessfulPreviousBuild MakeActions{..} _buildPlan moduleName = do
  fmap snd $ readExterns moduleName

-- | Constructs a BuildPlan for the given module graph.
--
-- The given MakeActions are used to collect various timestamps in order to
-- determine whether a module needs rebuilding.
construct
  :: forall m. MonadBaseControl IO m
  => MakeActions m
  -> CacheDb
  -> ([CST.PartialResult Module], [(ModuleName, [ModuleName])])
  -> m (BuildPlan, CacheDb)
construct MakeActions{..} cacheDb (sorted, graph) = do
  let sortedModuleNames = map (getModuleName . CST.resPartial) sorted
  cacheChanged <- A.forConcurrently sortedModuleNames getRebuildStatusIsUpToDate
  let prebuilt = M.empty
  case foldl (&&) True cacheChanged of
    True -> do
      let buildJobs = M.empty
      let dirty = M.empty
      env <- C.newMVar primEnv
      idx <- C.newMVar 1
      pure
        ( BuildPlan prebuilt M.empty M.empty env idx M.empty M.empty
        , cacheDb
        )
    False -> do
      rebuildStatuses <- A.forConcurrently sortedModuleNames getRebuildStatus
            -- foldl' collectPrebuiltModules M.empty $
            --   mapMaybe (\s -> (statusModuleName s, statusRebuildNever s,) <$> statusPrebuilt s) rebuildStatuses
      let dirty =
            foldl' collectDirtyModules M.empty $
              mapMaybe (\s -> (statusModuleName s, statusRebuildNever s,) <$> statusPrebuilt s) rebuildStatuses
      let toBeRebuilt = filter (not . flip M.member prebuilt) sortedModuleNames
      buildJobs <- foldM makeBuildJob M.empty toBeRebuilt
      cacheResults <- foldM (\m mn -> (\mvar -> M.insert mn mvar m) <$> newEmptyMVar) M.empty toBeRebuilt
      externsResults <- foldM (\m mn -> (\mvar -> M.insert mn mvar m) <$> newMVar Nothing) M.empty toBeRebuilt
      env <- C.newMVar primEnv
      idx <- C.newMVar 1
      pure
        ( BuildPlan prebuilt dirty buildJobs env idx cacheResults externsResults
        , let
            update = flip $ \s ->
              M.alter (const (statusNewCacheInfo s)) (statusModuleName s)
          in
            foldl' update cacheDb rebuildStatuses
        )
  where
    makeBuildJob prev moduleName = do
      buildJob <- BuildJob <$> C.newEmptyMVar
      pure (M.insert moduleName buildJob prev)

    getRebuildStatus :: ModuleName -> m RebuildStatus
    getRebuildStatus moduleName = do
      inputInfo <- getInputTimestampsAndHashes moduleName
      -- caching trace ("getRebuildStatus: " <> T.unpack (runModuleName moduleName) <> " " <> show (fmap (fmap (fmap (const ()))) inputInfo)) $
      case inputInfo of
        Left RebuildNever -> do
          dirtyExterns <- snd <$> readExterns moduleName
          prebuilt <- findExistingExtern dirtyExterns moduleName
          pure (RebuildStatus
            { statusModuleName = moduleName
            , statusRebuildNever = True
            , statusPrebuilt = prebuilt
            , statusDirtyExterns = dirtyExterns
            , statusNewCacheInfo = Nothing
            })
        Left RebuildAlways -> do
          pure (RebuildStatus
            { statusModuleName = moduleName
            , statusRebuildNever = False
            , statusPrebuilt = Nothing
            , statusDirtyExterns = Nothing
            , statusNewCacheInfo = Nothing
            })
        Right cacheInfo -> do
          cwd <- liftBase getCurrentDirectory
          (newCacheInfo, isUpToDate) <- checkChanged cacheDb moduleName cwd cacheInfo
          dirtyExterns <- snd <$> readExterns moduleName
          prebuilt <-
            if isUpToDate
              then findExistingExtern dirtyExterns moduleName
              else pure Nothing
          pure (RebuildStatus
            { statusModuleName = moduleName
            , statusRebuildNever = False
            , statusPrebuilt = prebuilt
            , statusDirtyExterns = dirtyExterns
            , statusNewCacheInfo = Just newCacheInfo
            })

    getRebuildStatusIsUpToDate :: ModuleName -> m Bool
    getRebuildStatusIsUpToDate moduleName = do
      inputInfo <- getInputTimestampsAndHashes moduleName
      case inputInfo of
        Left RebuildNever ->
          pure True
        Left RebuildAlways ->
          pure False
        Right cacheInfo -> do
          cwd <- liftBase getCurrentDirectory
          (newCacheInfo, isUpToDate) <- checkChanged cacheDb moduleName cwd cacheInfo
          pure isUpToDate

    findExistingExtern :: Maybe ExternsFile -> ModuleName -> m (Maybe Prebuilt)
    findExistingExtern mexterns moduleName = runMaybeT $ do
      timestamp <- MaybeT $ getOutputTimestamp moduleName
      externs <- MaybeT $ pure mexterns
      pure (Prebuilt timestamp externs)

    collectPrebuiltModules :: M.Map ModuleName Prebuilt -> (ModuleName, Bool, Prebuilt) -> M.Map ModuleName Prebuilt
    collectPrebuiltModules prev (moduleName, rebuildNever, pb)
      | rebuildNever = M.insert moduleName pb prev
      | otherwise = do
          let deps = fromMaybe (internalError "make: module not found in dependency graph.") (lookup moduleName graph)
          case traverse (fmap pbModificationTime . flip M.lookup prev) deps of
            Nothing ->
              -- If we end up here, one of the dependencies didn't exist in the
              -- prebuilt map and so we know a dependency needs to be rebuilt, which
              -- means we need to be rebuilt in turn.
              prev
            Just modTimes ->
              case maximumMaybe modTimes of
                Just depModTime | pbModificationTime pb < depModTime ->
                  prev
                _ -> M.insert moduleName pb prev

    collectDirtyModules :: M.Map ModuleName CacheFilesAvailable -> (ModuleName, Bool, Prebuilt) -> M.Map ModuleName CacheFilesAvailable
    collectDirtyModules prev (moduleName, rebuildNever, pb)
      | rebuildNever = M.insert moduleName (UpToDate pb) prev
      | otherwise = do
          let deps = fromMaybe (internalError "make: module not found in dependency graph.") (lookup moduleName graph)
          case traverse (fmap pbModificationTime . cfaPrebuilt <=< flip M.lookup prev) deps of
            Nothing ->
              -- If we end up here, one of the dependencies didn't exist in the
              -- prebuilt map and so we know a dependency needs to be rebuilt, which
              -- means we need to be rebuilt in turn.
              M.insert moduleName (DepChanged pb) prev
            Just modTimes ->
              case maximumMaybe modTimes of
                Just depModTime | pbModificationTime pb < depModTime ->
                  M.insert moduleName (DepChanged pb) prev -- NOTE[drathier]: hard-coded source changed to depchanged, so we can skip the transitive timestamp check here and only rely on the later timestamp+hash+externs caching
                _ -> M.insert moduleName (UpToDate pb) prev


anyDepChanged :: forall m. (MonadBaseControl IO m) => ModuleName -> [(ModuleName, [ModuleName])] -> BuildPlan -> m RebuildInstructions
anyDepChanged moduleName graph buildPlan = do
  let deps = fromMaybe (internalError "make: module not found in dependency graph.") (lookup moduleName graph)
  let f things =
        case things of
          [] -> pure FullDepsCacheHit
          a:ax -> do
            let mvar =
                  fromMaybe (internalError (show ("BuildPlan: module not in deps", a, moduleName, deps)))
                    $ M.lookup a (bpCacheResult buildPlan)
            cacheResult <- DH.hasLocked ("9.anyDepsChanged", moduleName, "depends on", a) $ readMVar mvar
            case cacheResult of
              Nothing -> pure (FailRebuildDepsFailed a)
              Just NoExternsChange -> f ax
              Just ExternsChanged -> pure (DepsChangedPleaseRebuildIfNeeded a)
  f deps

-- | Constructs a BuildPlan for the given module graph.
--
-- The given MakeActions are used to collect various timestamps in order to
-- determine whether a module needs rebuilding.
construct2
  :: forall m. MonadBaseControl IO m
  => MakeActions m
  -> CacheDb
  -> ([CST.PartialResult Module], [(ModuleName, [ModuleName])])
  -> m (BuildPlan, CacheDb)
construct2 MakeActions{..} cacheDb (sorted, graph) = do
  let sortedModuleNames = map (getModuleName . CST.resPartial) sorted
  let buildJobs = M.empty
  let dirty = M.empty
  let prebuilt = M.empty
  env <- C.newMVar primEnv
  idx <- C.newMVar 1
  let makeBuildJob prev moduleName = do
        buildJob <- BuildJob <$> C.newEmptyMVar
        pure (M.insert moduleName buildJob prev)
  buildJobs <- foldM makeBuildJob M.empty sortedModuleNames
  mapOfEmptyCacheResults <- foldM (\m mn -> (\v -> M.insert mn v m) <$> newEmptyMVar) M.empty sortedModuleNames
  mapOfEmptyExternResults <- foldM (\m mn -> (\v -> M.insert mn v m) <$> newMVar Nothing) M.empty sortedModuleNames
  pure
    ( BuildPlan prebuilt M.empty buildJobs env idx mapOfEmptyCacheResults mapOfEmptyExternResults
    , cacheDb
    )



maximumMaybe :: Ord a => [a] -> Maybe a
maximumMaybe [] = Nothing
maximumMaybe xs = Just $ maximum xs
