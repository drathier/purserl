module Command.Compile2 (go) where

import Prelude

import Control.Applicative (Alternative(..))
import Control.Monad (when)
import Data.Aeson qualified as A
import Data.Bool (bool)
import Data.ByteString.Lazy.UTF8 qualified as LBU8
import Data.List (intercalate, (\\))
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Traversable (for)
import Language.PureScript qualified as P
import Language.PureScript.CST qualified as CST
import Language.PureScript.Errors.JSON (JSONResult(..), toJSONErrors)
import Language.PureScript.Glob (toInputGlobs, PSCGlobs(..), warnFileTypeNotFound)
import Language.PureScript.Make (buildMakeActions, inferForeignModules, runMake)
import Options.Applicative qualified as Opts
import SharedCLI qualified
import System.Console.ANSI qualified as ANSI
import System.Exit (exitSuccess, exitFailure)
import System.Directory (getCurrentDirectory)
import System.FilePath.Glob (glob)
import System.IO (hPutStr, hPutStrLn, stderr, stdout)
import System.IO.UTF8 (readUTF8FilesT)
-- caching fork
import Data.IORef
import Control.Concurrent (threadDelay)
import Control.Exception (catch)
import System.Exit (ExitCode(..))
import           System.Environment (lookupEnv)
import qualified Data.HashMap.Strict as MS

import qualified System.Environment as System.Environment

import qualified Data.Maybe as Data.Maybe
import System.IO.Unsafe (unsafePerformIO)

data PSCMakeOptions = PSCMakeOptions
  { pscmInput        :: [FilePath]
  , pscmInputFromFile :: Maybe FilePath
  , pscmExclude      :: [FilePath]
  , pscmOutputDir    :: FilePath
  , pscmOpts         :: P.Options
  , pscmUsePrefix    :: Bool
  , pscmJSONErrors   :: Bool
  }

-- | Arguments: verbose, use JSON, warnings, errors
printWarningsAndErrors :: Bool -> Bool -> [(FilePath, T.Text)] -> P.MultipleErrors -> Either P.MultipleErrors a -> IO ()
printWarningsAndErrors verbose False files warnings errors = do
  pwd <- getCurrentDirectory

  probablySupportsANSI <- ANSI.hSupportsANSI stderr
  colorOverride <- (/=) "" <$> Data.Maybe.fromMaybe "" <$> System.Environment.lookupEnv "PURS_FORCE_COLOR"
  let cc = if colorOverride || probablySupportsANSI
           then Just P.defaultCodeColor
           else Nothing

  let ppeOpts = P.defaultPPEOptions { P.ppeCodeColor = cc, P.ppeFull = verbose, P.ppeRelativeDirectory = pwd, P.ppeFileContents = files }
  when (P.nonEmpty warnings) $
    putStrLn (P.prettyPrintMultipleWarnings ppeOpts warnings)
  case errors of
    Left errs -> do
      putStrLn (P.prettyPrintMultipleErrors ppeOpts errs)
      exitFailure
    Right _ -> return ()
printWarningsAndErrors verbose True files warnings errors = do
  colorOverride <- (/=) "" <$> Data.Maybe.fromMaybe "" <$> System.Environment.lookupEnv "PURS_FORCE_COLOR"
  let cc = if colorOverride
           then Just P.defaultCodeColor
           else Nothing

  putStrLn . LBU8.toString . A.encode $
    JSONResult (toJSONErrors cc verbose P.Warning files warnings)
               (either (toJSONErrors cc verbose P.Error files) (const []) errors)
  either (const exitFailure) (const (return ())) errors

go :: IO ()
go = do
  res <- compileImpl
    `catch`
      (\case
        ExitFailure code -> pure code
        ExitSuccess -> pure 0
      )

  case res of
    0 -> pure ()
    _ -> exitFailure


compileImpl :: IO Int
compileImpl = do
  -- let input = ["lib/Spago/**/*.purs"]
  -- let input = ["lib/Pluralizer.purs"]
  -- let input = ["/Users/drathier/lesslie/code/pay-backend/lib/Pluralizer.purs"]
  let input = ["/Users/drathier/lesslie/code/pay-backend/lib/Proxy.purs", "/Users/drathier/lesslie/code/pay-backend/lib/Spago/Type/Proxy.purs"]
  moduleFiles <- readUTF8FilesT input
  (makeErrors, makeWarnings) <- runMake P.defaultOptions $ do
    ms <- CST.parseModulesFromFiles id moduleFiles
    let filePathMap = M.fromList $ map (\(fp, pm) -> (P.getModuleName $ CST.resPartial pm, Right fp)) ms
    foreigns <- inferForeignModules filePathMap
    let makeActions = buildMakeActions "output2" filePathMap foreigns True Nothing
    P.make makeActions (map snd ms)

  printWarningsAndErrors (P.optionsVerboseErrors P.defaultOptions) False moduleFiles makeWarnings makeErrors
  exitSuccess

outputDirectory :: Opts.Parser FilePath
outputDirectory = Opts.strOption $
     Opts.short 'o'
  <> Opts.long "output"
  <> Opts.value "output"
  <> Opts.showDefault
  <> Opts.help "The output directory"

comments :: Opts.Parser Bool
comments = Opts.switch $
     Opts.short 'c'
  <> Opts.long "comments"
  <> Opts.help "Include comments in the generated code"

verboseErrors :: Opts.Parser Bool
verboseErrors = Opts.switch $
     Opts.short 'v'
  <> Opts.long "verbose-errors"
  <> Opts.help "Display verbose error messages"

noPrefix :: Opts.Parser Bool
noPrefix = Opts.switch $
     Opts.short 'p'
  <> Opts.long "no-prefix"
  <> Opts.help "Do not include comment header"

jsonErrors :: Opts.Parser Bool
jsonErrors = Opts.switch $
     Opts.long "json-errors"
  <> Opts.help "Print errors to stderr as JSON"

codegenTargets :: Opts.Parser [P.CodegenTarget]
codegenTargets = Opts.option targetParser $
     Opts.short 'g'
  <> Opts.long "codegen"
  <> Opts.value
      [ case unsafePerformIO (System.Environment.lookupEnv "PURS_CODEGEN_JS") of
         Nothing -> P.Erl
         Just _ -> P.JS
      ]
  <> Opts.help
      ( "Specifies comma-separated codegen targets to include. "
      <> targetsMessage
      <> " The default target is 'js', but if this option is used only the targets specified will be used."
      )

targetsMessage :: String
targetsMessage = "Accepted codegen targets are '" <> intercalate "', '" (M.keys P.codegenTargets) <> "'."

targetParser :: Opts.ReadM [P.CodegenTarget]
targetParser =
  Opts.str >>= \s ->
    for (T.split (== ',') s)
      $ maybe (Opts.readerError targetsMessage) pure
      . flip M.lookup P.codegenTargets
      . T.unpack
      . T.strip

options :: Opts.Parser P.Options
options =
  P.Options
    <$> verboseErrors
    <*> (not <$> comments)
    <*> (handleTargets <$> codegenTargets)
  where
    -- Ensure that the JS target is included if sourcemaps are
    handleTargets :: [P.CodegenTarget] -> S.Set P.CodegenTarget
    handleTargets ts = S.fromList (if P.JSSourceMap `elem` ts then P.JS : ts else ts)

pscMakeOptions :: Opts.Parser PSCMakeOptions
pscMakeOptions = PSCMakeOptions <$> many SharedCLI.inputFile
                                <*> SharedCLI.globInputFile
                                <*> many SharedCLI.excludeFiles
                                <*> outputDirectory
                                <*> options
                                <*> (not <$> noPrefix)
                                <*> jsonErrors
