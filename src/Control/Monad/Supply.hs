-- |
-- Fresh variable supply
--
module Control.Monad.Supply where

import Prelude

import Control.Applicative
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Functor.Identity

newtype SupplyT m a = SupplyT { unSupplyT :: StateT Integer m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadError e, MonadWriter w, MonadReader r, Alternative, MonadPlus)

runSupplyT :: Integer -> SupplyT m a -> m (a, Integer)
runSupplyT n = flip runStateT n . unSupplyT

mapSupplyT :: (m (a, Integer) -> m' (a', Integer)) -> SupplyT m a -> SupplyT m' a'
mapSupplyT f s = SupplyT (mapStateT f (unSupplyT s))

evalSupplyT :: (Functor m) => Integer -> SupplyT m a -> m a
evalSupplyT n = fmap fst . runSupplyT n

type Supply = SupplyT Identity

runSupply :: Integer -> Supply a -> (a, Integer)
runSupply n = runIdentity . runSupplyT n

instance (MonadIO m) => MonadIO (SupplyT m) where
    liftIO = lift . liftIO
