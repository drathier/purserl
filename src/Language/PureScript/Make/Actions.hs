module Language.PureScript.Make.Actions
  ( MakeActions(..)
  , RebuildPolicy(..)
  , ProgressMessage(..)
  , renderProgressMessage
  , buildMakeActions
  , checkForeignDecls
  , cacheDbFile
  , readCacheDb'
  , writeCacheDb'
  , ffiCodegen'
  ) where

import           Prelude

import           Control.Monad hiding (sequence)
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.IO.Class
import           Control.Monad.Reader (asks)
import           Control.Monad.Supply
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Writer.Class (MonadWriter(..))
import           Data.Aeson (Value(String), (.=), object)
import           Data.Bifunctor (bimap, first)
import           Data.Either (partitionEithers)
import           Data.Foldable (for_)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import           Data.Time.Clock (UTCTime)
import           Data.Version (showVersion)
import qualified Language.JavaScript.Parser as JS
import           Language.PureScript.AST
import qualified Language.PureScript.Bundle as Bundle
import qualified Language.PureScript.CodeGen.JS as J
import           Language.PureScript.CodeGen.JS.Printer
import qualified Language.PureScript.CoreFn as CF
import qualified Language.PureScript.CoreFn.ToJSON as CFJ
import           Language.PureScript.Crash
import qualified Language.PureScript.CST as CST
import qualified Language.PureScript.Docs.Prim as Docs.Prim
import qualified Language.PureScript.Docs.Types as Docs
import           Language.PureScript.Errors
import           Language.PureScript.Externs (ExternsFile, externsFileName)
import           Language.PureScript.Make.Monad
import           Language.PureScript.Make.Cache
import           Language.PureScript.Names
import           Language.PureScript.Options hiding (codegenTargets)
import           Language.PureScript.Pretty.Common (SMap(..))
import qualified Paths_purescript as Paths
import           SourceMap
import           SourceMap.Types
import           System.Directory (getCurrentDirectory)
import           System.FilePath ((</>), makeRelative, splitPath, normalise, splitDirectories)
import qualified System.FilePath.Posix as Posix
import           System.IO (stderr)
-- purerl
import Language.PureScript.Erl.CodeGen (buildCodegenEnvironment)
import Language.PureScript.AST as P
import Language.PureScript.Comments as P
import Language.PureScript.Crash as P
import Language.PureScript.Environment as P
import Language.PureScript.Errors as P hiding (indent)
import Language.PureScript.Externs as P
import Language.PureScript.Linter as P
import Language.PureScript.ModuleDependencies as P
import Language.PureScript.Names as P
import Language.PureScript.Options as P
import Language.PureScript.Pretty as P
import Language.PureScript.Renamer as P
import Language.PureScript.Roles as P
import Language.PureScript.Sugar as P
import Language.PureScript.TypeChecker as P
import Language.PureScript.Types as P

import Data.Maybe (catMaybes)

import qualified Build as Erl.Build

import Data.Either (fromRight)
import           Language.PureScript.Erl.Parser (parseFile)
import           Data.List ((\\))
import           Language.PureScript.Erl.CodeGen (moduleToErl, CodegenEnvironment)
import           Language.PureScript.Erl.CodeGen.Optimizer (optimize)
import           Language.PureScript.Erl.Pretty (prettyPrintErl)
import           Language.PureScript.Erl.CodeGen.Common (erlModuleName, erlModuleNameBase, atomModuleName, atom, ModuleType(..), runAtom)

import qualified Control.Monad.Trans.Except as ExceptT
import qualified Control.Monad.Trans.Reader as ReaderT
import qualified Control.Monad.Supply as SupplyT
import qualified Control.Monad.Logger as Logger
import qualified Language.PureScript.Erl.Errors
import qualified Language.PureScript.Erl.Make.Monad


--

import Data.IORef as IORef

-- | Determines when to rebuild a module
data RebuildPolicy
  -- | Never rebuild this module
  = RebuildNever
  -- | Always rebuild this module
  | RebuildAlways
  deriving (Show, Eq, Ord)

-- | Progress messages from the make process
data ProgressMessage
  = CompilingModule ModuleName (Maybe (Int, Int))
  -- ^ Compilation started for the specified module
  deriving (Show, Eq, Ord)

-- | Render a progress message
renderProgressMessage :: T.Text -> ProgressMessage -> T.Text
renderProgressMessage infx (CompilingModule mn mi) =
  T.concat
    [ renderProgressIndex mi
    , infx
    , runModuleName mn
    ]
  where
  renderProgressIndex :: Maybe (Int, Int) -> T.Text
  renderProgressIndex = maybe "" $ \(start, end) ->
    let start' = T.pack (show start)
        end' = T.pack (show end)
        preSpace = T.replicate (T.length end' - T.length start') " "
    in "[" <> preSpace <> start' <> " of " <> end' <> "] "

-- | Actions that require implementations when running in "make" mode.
--
-- This type exists to make two things abstract:
--
-- * The particular backend being used (JavaScript, C++11, etc.)
--
-- * The details of how files are read/written etc.
data MakeActions m = MakeActions
  { getInputTimestampsAndHashes :: ModuleName -> m (Either RebuildPolicy (M.Map FilePath (UTCTime, m ContentHash)))
  -- ^ Get the timestamps and content hashes for the input files for a module.
  -- The content hash is returned as a monadic action so that the file does not
  -- have to be read if it's not necessary.
  , getOutputTimestamp :: ModuleName -> m (Maybe UTCTime)
  -- ^ Get the time this module was last compiled, provided that all of the
  -- requested codegen targets were also produced then. The defaultMakeActions
  -- implementation uses the modification time of the externs file, because the
  -- externs file is written first and we always write one. If there is no
  -- externs file, or if any of the requested codegen targets were not produced
  -- the last time this module was compiled, this function must return Nothing;
  -- this indicates that the module will have to be recompiled.
  , touchOutputTimestamp :: ModuleName -> m (Maybe ())
  -- ^ Set the time this module was last compiled to the current time. Similar
  -- to and used with getOutputTimestamp.
  , readExterns :: ModuleName -> m (FilePath, Maybe ExternsFile)
  -- ^ Read the externs file for a module as a string and also return the actual
  -- path for the file.
  , codegen :: Environment -> CF.Module CF.Ann -> Docs.Module -> ExternsFile -> SupplyT m ()
  -- ^ Run the code generator for the module and write any required output files.
  , ffiCodegen :: CF.Module CF.Ann -> m ()
  -- ^ Check ffi and print it in the output directory.
  , progress :: ProgressMessage -> m ()
  -- ^ Respond to a progress update.
  , readCacheDb :: m CacheDb
  -- ^ Read the cache database (which contains timestamps and hashes for input
  -- files) from some external source, e.g. a file on disk.
  , writeCacheDb :: CacheDb -> m ()
  -- ^ Write the given cache database to some external source (e.g. a file on
  -- disk).
  , writePackageJson :: m ()
  -- ^ Write to the output directory the package.json file allowing Node.js to
  -- load .js files as ES modules.
  , outputPrimDocs :: m ()
  -- ^ If generating docs, output the documentation for the Prim modules
  }

-- | Given the output directory, determines the location for the
-- CacheDb file
cacheDbFile :: FilePath -> FilePath
cacheDbFile = (</> "cache-db.json")

readCacheDb'
  :: (MonadIO m, MonadError MultipleErrors m)
  => FilePath
  -- ^ The path to the output directory
  -> m CacheDb
readCacheDb' outputDir =
  fromMaybe mempty <$> readJSONFile (cacheDbFile outputDir)

writeCacheDb'
  :: (MonadIO m, MonadError MultipleErrors m)
  => FilePath
  -- ^ The path to the output directory
  -> CacheDb
  -- ^ The CacheDb to be written
  -> m ()
writeCacheDb' = writeJSONFile . cacheDbFile

writePackageJson'
  :: (MonadIO m, MonadError MultipleErrors m)
  => FilePath
  -- ^ The path to the output directory
  -> m ()
writePackageJson' outputDir = writeJSONFile (outputDir </> "package.json") $ object
  [ "type" .= String "module"
  ]

-- | A set of make actions that read and write modules from the given directory.
buildMakeActions
  :: FilePath
  -- ^ the output directory
  -> M.Map ModuleName (Either RebuildPolicy FilePath)
  -- ^ a map between module names and paths to the file containing the PureScript module
  -> M.Map ModuleName FilePath
  -- ^ a map between module name and the file containing the foreign javascript for the module
  -> Bool
  -- ^ Generate a prefix comment?
  -> Maybe ExternsMemCache
  -- ^ Optional memcache of already parsed externs files, for repeated builds
  -> MakeActions Make
buildMakeActions outputDir filePathMap foreigns usePrefix mExternsMemCache =
    MakeActions getInputTimestampsAndHashes getOutputTimestamp touchOutputTimestamp readExterns codegen ffiCodegen progress readCacheDb writeCacheDb writePackageJson outputPrimDocs
  where

  getInputTimestampsAndHashes
    :: ModuleName
    -> Make (Either RebuildPolicy (M.Map FilePath (UTCTime, Make ContentHash)))
  getInputTimestampsAndHashes mn = do
    let path = fromMaybe (internalError "Module has no filename in 'make'") $ M.lookup mn filePathMap
    case path of
      Left policy ->
        return (Left policy)
      Right filePath -> do
        cwd <- makeIO "Getting the current directory" getCurrentDirectory
        let inputPaths = map (normaliseForCache cwd) (filePath : maybeToList (M.lookup mn foreigns))
            getInfo fp = do
              ts <- getTimestamp fp
              return (ts, hashFile fp)
        pathsWithInfo <- traverse (\fp -> (fp,) <$> getInfo fp) inputPaths
        return $ Right $ M.fromList pathsWithInfo

  outputFilename :: ModuleName -> String -> FilePath
  outputFilename mn fn =
    let filePath = T.unpack (runModuleName mn)
    in outputDir </> filePath </> fn

  targetFilename :: ModuleName -> CodegenTarget -> FilePath
  targetFilename mn = \case
    JS -> outputFilename mn "index.js"
    JSSourceMap -> outputFilename mn "index.js.map"
    CoreFn -> outputFilename mn "corefn.json"
    Docs -> outputFilename mn "docs.json"
    Erl -> outFile mn

  getOutputTimestamp :: ModuleName -> Make (Maybe UTCTime)
  getOutputTimestamp mn = do
    codegenTargets <- asks optionsCodegenTargets
    mExternsTimestamp <- getTimestampMaybe (outputFilename mn externsFileName)
    case mExternsTimestamp of
      Nothing ->
        -- If there is no externs file, we will need to compile the module in
        -- order to produce one.
        pure Nothing
      Just externsTimestamp ->
        case NEL.nonEmpty (fmap (targetFilename mn) (S.toList codegenTargets)) of
          Nothing ->
            -- If the externs file exists and no other codegen targets have
            -- been requested, then we can consider the module up-to-date
            pure (Just externsTimestamp)
          Just outputPaths -> do
            -- If any of the other output paths are nonexistent or older than
            -- the externs file, then they should be considered outdated, and
            -- so the module will need rebuilding.
            mmodTimes <- traverse getTimestampMaybe outputPaths
            pure $ case sequence mmodTimes of
              Nothing ->
                Nothing
              Just modTimes ->
                if externsTimestamp <= minimum modTimes
                  then Just externsTimestamp
                  else Nothing

  touchOutputTimestamp :: ModuleName -> Make (Maybe ())
  touchOutputTimestamp mn = do
    -- first check that all relevant files exist, by fetching the timestamp of the existing cache files
    externsTimestamp <- getOutputTimestamp mn
    -- then touch them all, to mark them as up-to-date. If this fails partway through, the next getOutputTimestamp will consider it incomplete.
    codegenTargets <- asks optionsCodegenTargets
    case externsTimestamp of
      Nothing -> pure Nothing
      Just _ -> do
        -- then, after reading all relevant files succeeded, we update their mtimes
        touchTimestampMaybe (outputFilename mn externsFileName)
        case NEL.nonEmpty (fmap (targetFilename mn) (S.toList codegenTargets)) of
          Nothing ->
            pure (Just ())
          Just outputPaths -> do
            mmodTimes <- traverse touchTimestampMaybe outputPaths
            pure $ sequence_ mmodTimes


  readExterns :: ModuleName -> Make (FilePath, Maybe ExternsFile)
  readExterns mn = do
    let path = outputDir </> T.unpack (runModuleName mn) </> externsFileName
    (path, ) <$> readExternsFile mExternsMemCache path

  outputPrimDocs :: Make ()
  outputPrimDocs = do
    codegenTargets <- asks optionsCodegenTargets
    when (S.member Docs codegenTargets) $ for_ Docs.Prim.primModules $ \docsMod@Docs.Module{..} ->
      writeJSONFile (outputFilename modName "docs.json") docsMod

-- ########################

  moduleDir mn = outputDir </> T.unpack (P.runModuleName mn)
  outFile mn = moduleDir mn </> T.unpack (erlModuleName mn PureScriptModule) ++ ".erl"
  outFileChecked mn = moduleDir mn </> T.unpack (erlModuleName mn PureScriptCheckedModule) ++ ".erl"
  hrlFile mn = moduleDir mn </> T.unpack (erlModuleNameBase mn) ++ ".hrl"
  foreignHrlFile mn = moduleDir mn </> T.unpack (erlModuleName mn ForeignModule) ++ ".hrl"



  codegen :: Environment -> CF.Module CF.Ann -> Docs.Module -> ExternsFile -> SupplyT Make ()
  codegen environment m docs exts = do
    let mn = CF.moduleName m
    lift $ writeCborFile mExternsMemCache (outputFilename mn externsFileName) exts
    codegenTargets <- lift $ asks optionsCodegenTargets
    when (S.member CoreFn codegenTargets) $ do
      let coreFnFile = targetFilename mn CoreFn
          json = CFJ.moduleToJSON Paths.version m
      lift $ writeJSONFile coreFnFile json
    when (S.member JS codegenTargets) $ do
      foreignInclude <- case mn `M.lookup` foreigns of
        Just _
          | not $ requiresForeign m -> do
              return Nothing
          | otherwise -> do
              return $ Just "./foreign.js"
        Nothing | requiresForeign m -> throwError . errorMessage' (CF.moduleSourceSpan m) $ MissingFFIModule mn
                | otherwise -> return Nothing
      rawJs <- J.moduleToJs m foreignInclude
      dir <- lift $ makeIO "get the current directory" getCurrentDirectory
      let sourceMaps = S.member JSSourceMap codegenTargets
          (pjs, mappings) = if sourceMaps then prettyPrintJSWithSourceMaps rawJs else (prettyPrintJS rawJs, [])
          jsFile = targetFilename mn JS
          mapFile = targetFilename mn JSSourceMap
          prefix = ["Generated by purs version " <> T.pack (showVersion Paths.version) | usePrefix]
          js = T.unlines $ map ("// " <>) prefix ++ [pjs]
          mapRef = if sourceMaps then "//# sourceMappingURL=index.js.map\n" else ""
      lift $ do
        writeTextFile jsFile (TE.encodeUtf8 $ js <> mapRef)
        when sourceMaps $ genSourceMap dir mapFile (length prefix) mappings
    when (S.member Docs codegenTargets) $ do
      lift $ writeJSONFile (outputFilename mn "docs.json") docs

    -- ### Purerl

    when (S.member Erl codegenTargets) $ do
      erlForeigns <- do
        (merls :: M.Map ModuleName (Maybe FilePath)) <- traverse Erl.Build.inferForeignModule' foreigns
        case sequence merls of
          Nothing -> internalError (show ("couldn't find some erl foreign", merls))
          Just v -> pure v

      -- generate the corefn
      let coreFnFile = targetFilename mn CoreFn
          json = CFJ.moduleToJSON Paths.version m
      lift $ writeJSONFile coreFnFile json

      let externsFiles = exts

      let  getForeigns :: String -> Make [(T.Text, Int)]
           getForeigns path = do
             -- liftIO $ putStrLn (show ("getForeigns", path))
             text <- readTextFile path
             let (exports, ignoreExports) = fromRight ([],[]) $ parseFile path text
             pure $ exports \\ ignoreExports


      -- purerl env
      -- let env = buildCodegenEnvironment $ foldr P.applyExternsFileToEnvironment P.initEnvironment (catMaybes externsFiles)
      let env = buildCodegenEnvironment environment

      -- and generate the erlang code
        -- codegen :: CodegenEnvironment -> CF.Module CF.Ann -> SupplyT Make ()
        -- codegen env m = do
      let mn = CF.moduleName m
      foreignExports <- lift $ case mn `M.lookup` erlForeigns of
        Just path
          | not $ requiresForeign m ->
              return []
          | otherwise ->
              getForeigns path
        Nothing ->
          return []

      (exports, typeDecls, foreignSpecs, rawErl, checkedExports, checkedRawErl, memoizable) <- do
        -- SupplyT.mapSupplyT
        --   (\(Language.PureScript.Erl.Make.Monad.Make m, i) ->
        --     (Make
        --       -- ExceptT
        --       $ ReaderT.mapReaderT
        --          (ExceptT.withExceptT
        --            (\(Language.PureScript.Erl.Errors.MultipleErrors errors) -> MultipleErrors [])
        --          )
        --       -- Logger
        --       $ ReaderT.mapReaderT
        --          (ExceptT.mapExceptT
        --            (Logger.contraMapLoggerErrors
        --               (\(MultipleErrors _) -> Language.PureScript.Erl.Errors.MultipleErrors [])
        --            )
        --          )
        --        m
        --     , i)
        --   )

        let f m =
              liftIO $ do
                (l, r) <-
                  Language.PureScript.Erl.Make.Monad.runMake
                    (Options False False codegenTargets)
                    m

                case (l, r) of
                  -- TODO[drathier]: don't throw away purerl errors and warnings
                  (Right a, _) -> pure a
                  (Left lerrs, rerrs) -> internalError (show (Language.PureScript.Erl.Errors.runMultipleErrors lerrs, rerrs))

        SupplyT.mapSupplyT
          f
          (moduleToErl env m foreignExports) -- :: SupplyT Language.PureScript.Erl.Make.Monad.Make


      optimized <- optimize exports memoizable rawErl
      checked <- optimize checkedExports memoizable checkedRawErl

      dir <- lift $ makeIO "get file info: ." getCurrentDirectory
      let makeAbsFile file = dir </> file
      let pretty = prettyPrintErl makeAbsFile optimized
          prettyChecked = prettyPrintErl makeAbsFile checked
          prettySpecs = prettyPrintErl makeAbsFile foreignSpecs
          prettyDecls = prettyPrintErl makeAbsFile typeDecls

      let
          prefix :: [T.Text]
          prefix = ["Generated by purerl version " <> T.pack (showVersion Paths.version) | usePrefix]
          -- directives :: [(T.Text, Int)] -> ModuleType -> [T.Text]
          directives exports' moduleType = [
            "-module(" <> atom (atomModuleName mn moduleType) <> ").",
            "-export([" <> T.intercalate ", " (map (\(f, a) -> runAtom f <> "/" <> T.pack (show a)) exports') <> "]).",
            "-compile(nowarn_shadow_vars).",
            "-compile(nowarn_unused_vars).",
            "-compile(nowarn_unused_function).",
            "-compile(no_auto_import).",
            includeHrl,
            "-ifndef(PURERL_MEMOIZE).",
            "-define(MEMOIZE(X), X).",
            "-else.",
            "-define(MEMOIZE, memoize).",
            "memoize(X) -> X.",
            "-endif."
            ]
          includeHrl :: T.Text
          includeHrl = "-include(\"./" <> erlModuleNameBase mn <> ".hrl\").\n"
      let erl :: T.Text = T.unlines $ map ("% " <>) prefix ++ directives exports PureScriptModule ++  [ pretty ]
      lift $ writeTextFile (outFile mn) $ TE.encodeUtf8 erl

      -- when generateChecked $ do
      --   let erlchecked :: T.Text = T.unlines $ map ("% " <>) prefix ++ directives checkedExports PureScriptCheckedModule ++  [ prettyChecked ]
      --   lift $ writeTextFile (outFileChecked mn) $ TE.encodeUtf8 erlchecked

      let hrl :: T.Text = T.unlines $ map ("% " <>) prefix ++ [ prettyDecls ]
      lift $ writeTextFile (hrlFile mn) $ TE.encodeUtf8 hrl

      let foreignHrl :: T.Text = T.unlines $ map ("% " <>) prefix ++ [ includeHrl, prettySpecs ]
      lift $ writeTextFile (foreignHrlFile mn) $ TE.encodeUtf8 foreignHrl

  ffiCodegen :: CF.Module CF.Ann -> Make ()
  ffiCodegen m = do
    codegenTargets <- asks optionsCodegenTargets
    ffiCodegen' foreigns codegenTargets (Just outputFilename) m

    -- purserl, inlined because we need values from closure
    when (S.member Erl codegenTargets) $ do
          erlForeigns <- do
            (merls :: M.Map ModuleName (Maybe FilePath)) <- traverse Erl.Build.inferForeignModule' foreigns
            case sequence merls of
              Nothing -> internalError (show ("couldn't find some erl foreign", merls))
              Just v -> pure v

          let mn = CF.moduleName m
              foreignFile = moduleDir mn </> T.unpack (erlModuleName mn ForeignModule) ++ ".erl"
          case mn `M.lookup` erlForeigns of
            Just path
              | not $ requiresForeign m ->
                  tell $ errorMessage $ UnnecessaryFFIModule mn path
              | otherwise -> pure ()
            Nothing -> do
              when (requiresForeign m) $ liftIO $ putStrLn (show ("PossiblyMissingFFIModule", "required", requiresForeign m, "mn", mn, "foreignFile", foreignFile, "modules", M.keys erlForeigns))
              when (requiresForeign m) $ throwError . errorMessage $ MissingFFIModule mn
          for_ (mn `M.lookup` erlForeigns) $ \path ->
            copyFile path foreignFile


  genSourceMap :: String -> String -> Int -> [SMap] -> Make ()
  genSourceMap dir mapFile extraLines mappings = do
    let pathToDir = iterate (".." Posix.</>) ".." !! length (splitPath $ normalise outputDir)
        sourceFile = case mappings of
                      (SMap file _ _ : _) -> Just $ pathToDir Posix.</> normalizeSMPath (makeRelative dir (T.unpack file))
                      _ -> Nothing
    let rawMapping = SourceMapping { smFile = "index.js", smSourceRoot = Nothing, smMappings =
      map (\(SMap _ orig gen) -> Mapping {
          mapOriginal = Just $ convertPos $ add 0 (-1) orig
        , mapSourceFile = sourceFile
        , mapGenerated = convertPos $ add (extraLines + 1) 0 gen
        , mapName = Nothing
        }) mappings
    }
    let mapping = generate rawMapping
    writeJSONFile mapFile mapping
    where
    add :: Int -> Int -> SourcePos -> SourcePos
    add n m (SourcePos n' m') = SourcePos (n + n') (m + m')

    convertPos :: SourcePos -> Pos
    convertPos SourcePos { sourcePosLine = l, sourcePosColumn = c } =
      Pos { posLine = fromIntegral l, posColumn = fromIntegral c }

    normalizeSMPath :: FilePath -> FilePath
    normalizeSMPath = Posix.joinPath . splitDirectories

  requiresForeign :: CF.Module a -> Bool
  requiresForeign = not . null . CF.moduleForeign

  progress :: ProgressMessage -> Make ()
  progress = liftIO . TIO.hPutStr stderr . (<> "\n") . renderProgressMessage "Compiling S20 "

  readCacheDb :: Make CacheDb
  readCacheDb = readCacheDb' outputDir

  writeCacheDb :: CacheDb -> Make ()
  writeCacheDb = writeCacheDb' outputDir

  writePackageJson :: Make ()
  writePackageJson = writePackageJson' outputDir

data ForeignModuleType = ESModule | CJSModule deriving (Show)

-- | Check that the declarations in a given PureScript module match with those
-- in its corresponding foreign module.
checkForeignDecls :: CF.Module ann -> FilePath -> Make (Either MultipleErrors (ForeignModuleType, S.Set Ident))
checkForeignDecls m path = do
  jsStr <- T.unpack <$> readTextFile path

  let
    parseResult :: Either MultipleErrors JS.JSAST
    parseResult = first (errorParsingModule . Bundle.UnableToParseModule) $ JS.parseModule jsStr path
  traverse checkFFI parseResult

  where
  mname = CF.moduleName m
  modSS = CF.moduleSourceSpan m

  checkFFI :: JS.JSAST -> Make (ForeignModuleType, S.Set Ident)
  checkFFI js = do
    (foreignModuleType, foreignIdentsStrs) <-
        case (,) <$> getForeignModuleExports js <*> getForeignModuleImports js of
          Left reason -> throwError $ errorParsingModule reason
          Right (Bundle.ForeignModuleExports{..}, Bundle.ForeignModuleImports{..})
            | not (null cjsExports && null cjsImports)
            , null esExports
            , null esImports -> do
                let deprecatedFFI = filter (elem '\'') cjsExports
                unless (null deprecatedFFI) $
                  errorDeprecatedForeignPrimes deprecatedFFI

                pure (CJSModule, cjsExports)
            | otherwise -> do
                unless (null cjsImports) $
                  errorUnsupportedFFICommonJSImports cjsImports

                unless (null cjsExports) $
                  errorUnsupportedFFICommonJSExports cjsExports

                pure (ESModule, esExports)

    foreignIdents <- either
                      errorInvalidForeignIdentifiers
                      (pure . S.fromList)
                      (parseIdents foreignIdentsStrs)
    let importedIdents = S.fromList (CF.moduleForeign m)

    let unusedFFI = foreignIdents S.\\ importedIdents
    unless (null unusedFFI) $
      tell . errorMessage' modSS . UnusedFFIImplementations mname $
        S.toList unusedFFI

    let missingFFI = importedIdents S.\\ foreignIdents
    unless (null missingFFI) $
      throwError . errorMessage' modSS . MissingFFIImplementations mname $
        S.toList missingFFI
    pure (foreignModuleType, foreignIdents)

  errorParsingModule :: Bundle.ErrorMessage -> MultipleErrors
  errorParsingModule = errorMessage' modSS . ErrorParsingFFIModule path . Just

  getForeignModuleExports :: JS.JSAST -> Either Bundle.ErrorMessage  Bundle.ForeignModuleExports
  getForeignModuleExports = Bundle.getExportedIdentifiers (T.unpack (runModuleName mname))

  getForeignModuleImports :: JS.JSAST -> Either Bundle.ErrorMessage Bundle.ForeignModuleImports
  getForeignModuleImports = Bundle.getImportedModules (T.unpack (runModuleName mname))

  errorInvalidForeignIdentifiers :: [String] -> Make a
  errorInvalidForeignIdentifiers =
    throwError . mconcat . map (errorMessage . InvalidFFIIdentifier mname . T.pack)

  errorDeprecatedForeignPrimes :: [String] -> Make a
  errorDeprecatedForeignPrimes =
    throwError . mconcat . map (errorMessage' modSS . DeprecatedFFIPrime mname . T.pack)

  errorUnsupportedFFICommonJSExports :: [String] -> Make a
  errorUnsupportedFFICommonJSExports =
    throwError . errorMessage' modSS . UnsupportedFFICommonJSExports mname . map T.pack

  errorUnsupportedFFICommonJSImports :: [String] -> Make a
  errorUnsupportedFFICommonJSImports =
    throwError . errorMessage' modSS . UnsupportedFFICommonJSImports mname . map T.pack

  parseIdents :: [String] -> Either [String] [Ident]
  parseIdents strs =
    case partitionEithers (map parseIdent strs) of
      ([], idents) ->
        Right idents
      (errs, _) ->
        Left errs

  -- We ignore the error message here, just being told it's an invalid
  -- identifier should be enough.
  parseIdent :: String -> Either String Ident
  parseIdent str =
    bimap (const str) (Ident . CST.getIdent . CST.nameValue . snd)
      . CST.runTokenParser CST.parseIdent
      . CST.lex
      $ T.pack str

-- | FFI check and codegen action.
-- If path maker is supplied copies foreign module to the output.
ffiCodegen'
  :: M.Map ModuleName FilePath
  -> S.Set CodegenTarget
  -> Maybe (ModuleName -> String -> FilePath)
  -> CF.Module CF.Ann
  -> Make ()
ffiCodegen' foreigns codegenTargets makeOutputPath m = do
  when (S.member JS codegenTargets) $ do
    let mn = CF.moduleName m
    case mn `M.lookup` foreigns of
      Just path
        | not $ requiresForeign m ->
            tell $ errorMessage' (CF.moduleSourceSpan m) $ UnnecessaryFFIModule mn path
        | otherwise -> do
            checkResult <- checkForeignDecls m path
            case checkResult of
              Left _ -> copyForeign path mn
              Right (ESModule, _) -> copyForeign path mn
              Right (CJSModule, _) -> do
                throwError $ errorMessage' (CF.moduleSourceSpan m) $ DeprecatedFFICommonJSModule mn path
      Nothing | requiresForeign m -> throwError . errorMessage' (CF.moduleSourceSpan m) $ MissingFFIModule mn
              | otherwise -> return ()
  where
  requiresForeign = not . null . CF.moduleForeign

  copyForeign path mn =
    for_ makeOutputPath (\outputFilename -> copyFile path (outputFilename mn "foreign.js"))
