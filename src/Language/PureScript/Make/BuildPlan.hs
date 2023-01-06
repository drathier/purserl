module Language.PureScript.Make.BuildPlan
  ( BuildPlan(bpEnv, bpIndex)
  , BuildJobResult(..)
  , buildJobSuccess
  , construct
  , getResult
  , getCacheFilesAvailable
  , shouldRecompile
  , collectResults
  , markComplete
  , needsRebuild
  ) where

import           Prelude

import           Control.Concurrent.Async.Lifted as A
import           Control.Concurrent.Lifted as C
import           Control.Monad.Base (liftBase)
import           Control.Monad hiding (sequence)
import           Control.Monad.Trans.Control (MonadBaseControl(..))
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import           Data.Foldable (foldl')
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Time.Clock (UTCTime)
import           Language.PureScript.AST
import           Language.PureScript.Crash
import qualified Language.PureScript.CST as CST
import           Language.PureScript.Errors
import           Language.PureScript.Externs
import           Language.PureScript.Make.Actions as Actions
import           Language.PureScript.Make.Cache
import           Language.PureScript.Names (ModuleName, runModuleName)
import           Language.PureScript.Sugar.Names.Env
import           System.Directory (getCurrentDirectory)
import qualified Data.Text as T
import Debug.Trace
import PrettyPrint

-- | The BuildPlan tracks information about our build progress, and holds all
-- prebuilt modules for incremental builds.
data BuildPlan = BuildPlan
  { bpPrebuilt :: M.Map ModuleName Prebuilt
  , bpDirtyExterns :: M.Map ModuleName CacheFilesAvailable
  , bpBuildJobs :: M.Map ModuleName BuildJob
  , bpEnv :: C.MVar Env
  , bpIndex :: C.MVar Int
  }

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

buildJobSuccess :: BuildJobResult -> Maybe (MultipleErrors, ExternsFile)
buildJobSuccess (BuildJobSucceeded warnings externs) = Just (warnings, externs)
buildJobSuccess _ = Nothing

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
  => BuildPlan
  -> ModuleName
  -> BuildJobResult
  -> m ()
markComplete buildPlan moduleName result = do
  let BuildJob rVar = fromMaybe (internalError "make: markComplete no barrier") $ M.lookup moduleName (bpBuildJobs buildPlan)
  putMVar rVar result

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
  barrierResults <- traverse (readMVar . bjResult) $ bpBuildJobs buildPlan
  pure (M.union prebuiltResults barrierResults)

-- | Gets the the build result for a given module name independent of whether it
-- was rebuilt or prebuilt. Prebuilt modules always return no warnings.
getResult
  :: (MonadBaseControl IO m)
  => BuildPlan
  -> ModuleName
  -> m (Maybe (MultipleErrors, ExternsFile))
getResult buildPlan moduleName =
  case M.lookup moduleName (bpPrebuilt buildPlan) of
    Just es ->
      pure (Just (MultipleErrors [], pbExternsFile es))
    Nothing -> do
      r <- readMVar $ bjResult $ fromMaybe (internalError "make: no barrier") $ M.lookup moduleName (bpBuildJobs buildPlan)
      pure $ buildJobSuccess r


data CacheFilesAvailable = DepChanged Prebuilt | SourceChanged | UpToDate Prebuilt
  -- TODO[drathier]: duplicate ExternsFile if UpToDate? Also stored in Prebuilt?
  --deriving (Show)
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

shouldRecompile :: ModuleName -> CacheFilesAvailable -> [ExternsFile] -> Either (Maybe ExternsFile) ExternsFile
shouldRecompile mn cfa externs = do
  -- let cfatag =
  --       case cfa of
  --         UpToDate _ -> "UpToDate"
  --         SourceChanged -> "SourceChanged"
  --         DepChanged _ -> "DepChanged"
  -- let !_ =
  --       case cfa of
  --         DepChanged _ -> ()
  --         _ -> trace (T.unpack (runModuleName mn) <> " shouldRecompile? " <> show cfatag) ()
  case cfa of
    UpToDate pb -> Right (pbExternsFile pb)
    SourceChanged -> Left Nothing
    DepChanged pb ->
      let oldExts = pbExternsFile pb in
      let old = efUpstreamCacheShapes oldExts in
      let shapesMap = M.intersectionWith (\_ s -> s) old $ M.fromList $ (\ef -> (efModuleName ef, efOurCacheShapes ef)) <$> externs in
      let !_ = if old == mempty then trace (show ("WARNING: module doesn't export anything at all!" :: String, mn)) () else () in

      let
          interestingDiff5 =
            M.differenceWith
              (\a b ->
                case dbOpaqueDiffDiff a b of
                  v | v == mempty -> Nothing
                  v -> Just v
              )
              old
              shapesMap
      in
      case interestingDiff5 == mempty of
        True ->
          -- trace (T.unpack (runModuleName mn) <> ": cache hit") $
          Right oldExts
        False ->
          -- trace (T.unpack (runModuleName mn) <> ": cache miss: " <> sShow interestingDiff5) $
          Left (Just oldExts)


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
  rebuildStatuses <- A.forConcurrently sortedModuleNames getRebuildStatus
  let prebuilt =
        foldl' collectPrebuiltModules M.empty $
          mapMaybe (\s -> (statusModuleName s, statusRebuildNever s,) <$> statusPrebuilt s) rebuildStatuses
  let dirty =
        foldl' collectDirtyModules M.empty $
          mapMaybe (\s -> (statusModuleName s, statusRebuildNever s,) <$> statusPrebuilt s) rebuildStatuses
  let toBeRebuilt = filter (not . flip M.member prebuilt) sortedModuleNames
  buildJobs <- foldM makeBuildJob M.empty toBeRebuilt
  env <- C.newMVar primEnv
  idx <- C.newMVar 1
  pure
    ( BuildPlan prebuilt dirty buildJobs env idx
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
                  M.insert moduleName SourceChanged prev
                _ -> M.insert moduleName (UpToDate pb) prev

maximumMaybe :: Ord a => [a] -> Maybe a
maximumMaybe [] = Nothing
maximumMaybe xs = Just $ maximum xs
