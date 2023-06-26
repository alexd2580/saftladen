{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.MVar (modifyState_, replaceState_, recomputeState_, withMVar) where

import Control.Concurrent.MVar (MVar, modifyMVar_, putMVar, takeMVar)
import Control.Monad.Trans (MonadIO, liftIO)
import Prelude

modifyState_ :: MVar s -> (s -> s) -> IO ()
modifyState_ shared transform = modifyMVar_ shared $ return . transform

replaceState_ :: MVar s -> s -> IO ()
replaceState_ shared = modifyState_ shared . const

recomputeState_ :: MVar s -> IO s -> IO ()
recomputeState_ shared = (>>= modifyState_ shared . const)

withMVar :: MonadIO m => MVar a -> (a -> m b) -> m b
withMVar mvar f = do
  content <- liftIO $ takeMVar mvar
  x <- f content
  liftIO $ putMVar mvar content
  return x
