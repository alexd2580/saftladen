{-# LANGUAGE NoImplicitPrelude #-}

module Utils.MVar where

import Control.Concurrent.MVar (MVar, modifyMVar_)
import Control.Monad (return, (>>=))
import Data.Function (const, ($), (.))
import System.IO (IO)

modifyState_ :: MVar s -> (s -> s) -> IO ()
modifyState_ shared transform = modifyMVar_ shared $ return . transform

replaceState_ :: MVar s -> s -> IO ()
replaceState_ shared = modifyState_ shared . const

recomputeState_ :: MVar s -> IO s -> IO ()
recomputeState_ shared = (>>= modifyState_ shared . const)
