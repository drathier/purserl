{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.PureScript.Erl.Make.Monad
  ( -- * Implementation of Make API using files on disk
    Make(..)
  , runMake
  , makeIO
  , getTimestamp
  , getTimestampMaybe
  , readTextFile
  , readJSONFile
  , readExternsFile
  , writeTextFile
  , writeJSONFile
  , copyFile
  , catchDoesNotExist
  , hashFile
  ) where

import           Prelude

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as Serialise
import           Control.Exception (tryJust, fromException, Exception (displayException))
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
import           Data.Time.Clock (UTCTime)
import           Language.PureScript.Erl.Errors
import           Language.PureScript.Erl.Errors.Types
import           Language.PureScript.Externs (ExternsFile)
import           Language.PureScript.Options
import           System.Directory (createDirectoryIfMissing, getModificationTime)
import qualified System.Directory as Directory
import           System.FilePath (takeDirectory)
import           System.IO.Error (tryIOError, isDoesNotExistError)
import           System.IO.UTF8 (readUTF8FileT)
import qualified Language.PureScript.Make.Cache as Cache

import Debug.Trace

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
  either (throwError . singleError . ErrorMessage [] . FileIOError description . Text.pack . displayException) pure res

-- | Get a file's modification time in the 'Make' monad, capturing any errors
-- using the 'MonadError' instance.
getTimestamp :: FilePath -> Make UTCTime
getTimestamp path =
  makeIO ("get a timestamp for file: " <> Text.pack path) $ getModificationTime path

-- | Get a file's modification time in the 'Make' monad, returning Nothing if
-- the file does not exist.
getTimestampMaybe :: FilePath -> Make (Maybe UTCTime)
getTimestampMaybe path =
  makeIO ("get a timestamp for file: " <> Text.pack path) $ catchDoesNotExist $ getModificationTime path

-- | Read a text file strictly in the 'Make' monad, capturing any errors using
-- the 'MonadError' instance.
readTextFile :: FilePath -> Make Text
readTextFile path =
  makeIO ("read file: " <> Text.pack path) $
    readUTF8FileT path

-- | Read a JSON file in the 'Make' monad, returning 'Nothing' if the file does
-- not exist or could not be parsed. Errors are captured using the 'MonadError'
-- instance.
readJSONFile :: Aeson.FromJSON a => FilePath -> Make (Maybe a)
readJSONFile path =
  makeIO ("read JSON file: " <> Text.pack path) $ do
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

catchDeserialiseFailure :: IO a -> IO (Maybe a)
catchDeserialiseFailure inner = do
  r <- tryJust fromException inner
  case r of
    Left (_ :: Serialise.DeserialiseFailure) ->
      return Nothing
    Right x ->
      return (Just x)

-- | Read an externs file, returning 'Nothing' if the file does not exist,
-- could not be parsed, or was generated by a different version of the
-- compiler.
readExternsFile :: FilePath -> Make (Maybe ExternsFile)
readExternsFile path = do
  readCborFile path

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

-- | Write a text file in the 'Make' monad, capturing any errors using the
-- 'MonadError' instance.
writeTextFile :: FilePath -> B.ByteString -> Make ()
writeTextFile path text = makeIO ("write file: " <> Text.pack path) $ do
  createParentDirectory path
  B.writeFile path text

-- | Write a JSON file in the 'Make' monad, capturing any errors using the
-- 'MonadError' instance.
writeJSONFile :: Aeson.ToJSON a => FilePath -> a -> Make ()
writeJSONFile path value = makeIO ("write JSON file: " <> Text.pack path) $ do
  createParentDirectory path
  Aeson.encodeFile path value

-- | Copy a file in the 'Make' monad, capturing any errors using the
-- 'MonadError' instance.
copyFile :: FilePath -> FilePath -> Make ()
copyFile src dest =
  trace (show ("Tried to copyFile with wrong copyFile function! This should never ever happen!", "src", src, "dest", dest)) $
  makeIO ("copy file: " <> Text.pack src <> " -> " <> Text.pack dest) $ do
    createParentDirectory dest
    Directory.copyFile src dest

createParentDirectory :: FilePath -> IO ()
createParentDirectory = createDirectoryIfMissing True . takeDirectory

hashFile :: (MonadIO m, MonadError MultipleErrors m) => FilePath -> m Cache.ContentHash
hashFile path = do
  makeIO ("hash file: " <> Text.pack path)
    (Cache.hash <$> B.readFile path)