{-# LANGUAGE FlexibleContexts #-}
module DH where

import Control.Exception
import Control.Monad.Base (liftBase)
import Control.Monad.Trans.Control (MonadBaseControl, control)
import Prelude

hasLocked :: (MonadBaseControl IO m) => Show meta => meta -> m a -> m a
hasLocked meta action = control $ \runInIO ->
  runInIO action `catches`
    [ Handler $ \exc@BlockedIndefinitelyOnMVar ->
        putStrLn ("[MVar]: " ++ show meta) >> throwIO exc
    , Handler $ \exc@BlockedIndefinitelyOnSTM ->
        putStrLn ("[STM]: " ++ show meta) >> throwIO exc
    ]
