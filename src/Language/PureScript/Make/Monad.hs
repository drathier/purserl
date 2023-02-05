module Language.PureScript.Make.Monad
  ( -- * Implementation of Make API using files on disk
    Make(..)
  , runMake
  , makeIO
  , getTimestamp
  , getTimestampMaybe
  , touchTimestampMaybe
  , readTextFile
  , readJSONFile
  , readJSONFileIO
  , readCborFile
  , readCborFileIO
  , readExternsFile
  , hashFile
  , writeTextFile
  , writeJSONFile
  , writeCborFile
  , writeCborFileIO
  , copyFile
  --
  , ExternsMemCache
  ) where

import           Prelude

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as Serialise
import           Control.Exception (fromException, tryJust)
import           Control.Monad (join, guard)
import           Control.Monad.Base (MonadBase(..))
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader (MonadReader(..), ReaderT(..))
import           Control.Monad.Trans.Control (MonadBaseControl(..))
import           Control.Monad.Trans.Except
import           Control.Monad.Writer.Class (MonadWriter(..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Language.PureScript.Errors
import           Language.PureScript.Externs (ExternsFile, externsIsCurrentVersion)
import           Language.PureScript.Make.Cache (ContentHash, hash)
import           Language.PureScript.Options
import           System.Directory (createDirectoryIfMissing, getModificationTime, setModificationTime)
import qualified System.Directory as Directory
import           System.FilePath (takeDirectory)
import           System.IO.Error (tryIOError, isDoesNotExistError)
import           System.IO.UTF8 (readUTF8FileT)

import Data.IORef as IORef
import qualified Data.Map as M
import qualified Data.ByteString as BS

-- | A monad for running make actions
newtype Make a = Make
  { unMake :: ReaderT Options (ExceptT MultipleErrors (Logger MultipleErrors)) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadError MultipleErrors, MonadWriter MultipleErrors, MonadReader Options)

instance MonadBase IO Make where
  liftBase = liftIO

instance MonadBaseControl IO Make where
  type StM Make a = Either MultipleErrors a
  liftBaseWith f = Make $ liftBaseWith $ \q -> f (q . unMake)
  restoreM = Make . restoreM

-- | Execute a 'Make' monad, returning either errors, or the result of the compile plus any warnings.
runMake :: Options -> Make a -> IO (Either MultipleErrors a, MultipleErrors)
runMake opts = runLogger' . runExceptT . flip runReaderT opts . unMake

-- | Run an 'IO' action in the 'Make' monad. The 'String' argument should
-- describe what we were trying to do; it is used for rendering errors in the
-- case that an IOException is thrown.
makeIO :: (MonadIO m, MonadError MultipleErrors m) => Text -> IO a -> m a
makeIO description io = do
  res <- liftIO (tryIOError io)
  either (throwError . singleError . ErrorMessage [] . FileIOError description) pure res

-- | Get a file's modification time in the 'Make' monad, capturing any errors
-- using the 'MonadError' instance.
getTimestamp :: (MonadIO m, MonadError MultipleErrors m) => FilePath -> m UTCTime
getTimestamp path =
  makeIO ("get a timestamp for file: " <> Text.pack path) $ getModificationTime path

-- | Get a file's modification time in the 'Make' monad, returning Nothing if
-- the file does not exist.
getTimestampMaybe :: (MonadIO m, MonadError MultipleErrors m) => FilePath -> m (Maybe UTCTime)
getTimestampMaybe path =
  makeIO ("get a timestamp for file: " <> Text.pack path) $ catchDoesNotExist $ getModificationTime path

-- | Touch a file's modification time in the 'Make' monad, returning Nothing if
-- the file does not exist.
touchTimestampMaybe :: (MonadIO m, MonadError MultipleErrors m) => FilePath -> m (Maybe ())
touchTimestampMaybe path =
  makeIO ("get a timestamp for file: " <> Text.pack path) $ catchDoesNotExist $ (getCurrentTime >>= setModificationTime path)

-- | Read a text file strictly in the 'Make' monad, capturing any errors using
-- the 'MonadError' instance.
readTextFile :: (MonadIO m, MonadError MultipleErrors m) => FilePath -> m Text
readTextFile path =
  makeIO ("read file: " <> Text.pack path) $
    readUTF8FileT path

-- | Read a JSON file in the 'Make' monad, returning 'Nothing' if the file does
-- not exist or could not be parsed. Errors are captured using the 'MonadError'
-- instance.
readJSONFile :: (MonadIO m, MonadError MultipleErrors m) => Aeson.FromJSON a => FilePath -> m (Maybe a)
readJSONFile path =
  makeIO ("read JSON file: " <> Text.pack path) (readJSONFileIO path)

readJSONFileIO :: Aeson.FromJSON a => FilePath -> IO (Maybe a)
readJSONFileIO path = do
  r <- catchDoesNotExist $ Aeson.decodeFileStrict' path
  return $ join r

-- | Read a Cbor encoded file in the 'Make' monad, returning
-- 'Nothing' if the file does not exist or could not be parsed. Errors
-- are captured using the 'MonadError' instance.
readCborFile :: (MonadIO m, MonadError MultipleErrors m) => Serialise a => FilePath -> m (Maybe a)
readCborFile path =
  makeIO ("read Binary file: " <> Text.pack path) (readCborFileIO path)

readCborFileIO :: Serialise a => FilePath -> IO (Maybe a)
readCborFileIO path = do
  r <- catchDoesNotExist $ catchDeserialiseFailure $ Serialise.readFileDeserialise path
  return (join r)

-- | Read an externs file, returning 'Nothing' if the file does not exist,
-- could not be parsed, or was generated by a different version of the
-- compiler.
readExternsFile :: (MonadIO m, MonadError MultipleErrors m) => Maybe ExternsMemCache -> FilePath -> m (Maybe ExternsFile)
readExternsFile mmemCacheRef path = do
  -- TODO[drathier]: hash externs file too, so we know if it changed
  mNewHash <- hashFileMaybe path
  mCache <-
    case mmemCacheRef of
      Nothing -> pure Nothing
      Just memCacheRef -> do
        liftIO $ IORef.atomicModifyIORef' memCacheRef (\x ->
            ( x
            , case M.lookup path x of
                Just (oldHash, externs) | Just oldHash == mNewHash -> pure externs
                _ -> Nothing
            )
          )
  case mCache of
    Just cache -> do
      -- liftIO $ putStrLn ("readExternsFile cache hit: " <> path)
      return (Just cache)
    Nothing -> do
      mexterns <- readExternsFileImpl path
      maybeWriteExternsToMemCache mmemCacheRef path mNewHash mexterns
      pure mexterns

type ExternsMemCache = IORef (M.Map FilePath (ContentHash, ExternsFile))

maybeWriteExternsToMemCache mmemCacheRef path mexternsHash mexterns = do
  case mmemCacheRef of
    Nothing -> pure ()
    Just memCacheRef -> do
      -- liftIO $ putStrLn ("readExternsFile cache write: " <> path)
      () <- liftIO $
        case mexternsHash of
          Nothing -> pure ()
          Just externsHash ->
            case mexterns of
              Nothing -> pure ()
              Just externs ->
                IORef.atomicModifyIORef' memCacheRef (\x -> (M.insert path (externsHash, externs) x, ()))
      pure ()

readExternsFileImpl :: (MonadIO m, MonadError MultipleErrors m) => FilePath -> m (Maybe ExternsFile)
readExternsFileImpl path = do
  mexterns <- readCborFile path
  return $ do
    externs <- mexterns
    guard $ externsIsCurrentVersion externs
    return externs

hashFile :: (MonadIO m, MonadError MultipleErrors m) => FilePath -> m ContentHash
hashFile path = do
  makeIO ("hash file: " <> Text.pack path)
    (hash <$> B.readFile path)

hashFileMaybe :: (MonadIO m, MonadError MultipleErrors m) => FilePath -> m (Maybe ContentHash)
hashFileMaybe path =
  makeIO ("hash file: " <> Text.pack path) $ catchDoesNotExist $ do
    hash <$> B.readFile path

-- | If the provided action threw an 'isDoesNotExist' error, catch it and
-- return Nothing. Otherwise return Just the result of the inner action.
catchDoesNotExist :: IO a -> IO (Maybe a)
catchDoesNotExist inner = do
  r <- tryJust (guard . isDoesNotExistError) inner
  case r of
    Left () ->
      return Nothing
    Right x ->
      return (Just x)

catchDeserialiseFailure :: IO a -> IO (Maybe a)
catchDeserialiseFailure inner = do
  r <- tryJust fromException inner
  case r of
    Left (_ :: Serialise.DeserialiseFailure) ->
      return Nothing
    Right x ->
      return (Just x)

-- | Write a text file in the 'Make' monad, capturing any errors using the
-- 'MonadError' instance.
writeTextFile :: FilePath -> B.ByteString -> Make ()
writeTextFile path text = makeIO ("write file: " <> Text.pack path) $ do
  createParentDirectory path
  B.writeFile path text

-- | Write a JSON file in the 'Make' monad, capturing any errors using the
-- 'MonadError' instance.
writeJSONFile :: (MonadIO m, MonadError MultipleErrors m) => Aeson.ToJSON a => FilePath -> a -> m ()
writeJSONFile path value = makeIO ("write JSON file: " <> Text.pack path) $ do
  createParentDirectory path
  Aeson.encodeFile path value

writeCborFile :: (MonadIO m, MonadError MultipleErrors m) => Maybe ExternsMemCache -> FilePath -> ExternsFile -> m ()
writeCborFile mmemCacheRef path value = do
  res <- makeIO ("write Cbor file: " <> Text.pack path) (writeCborFileIO path value)
  mNewHash <- hashFileMaybe path
  maybeWriteExternsToMemCache mmemCacheRef path mNewHash (Just value)
  pure res

writeCborFileIO :: Serialise a => FilePath -> a -> IO ()
writeCborFileIO path value = do
  createParentDirectory path
  Serialise.writeFileSerialise path value

-- | Copy a file in the 'Make' monad, capturing any errors using the
-- 'MonadError' instance.
copyFile :: (MonadIO m, MonadError MultipleErrors m) => FilePath -> FilePath -> m ()
copyFile src dest =
  makeIO ("copy file: " <> Text.pack src <> " -> " <> Text.pack dest) $ do
    createParentDirectory dest
    Directory.copyFile src dest

createParentDirectory :: FilePath -> IO ()
createParentDirectory = createDirectoryIfMissing True . takeDirectory
