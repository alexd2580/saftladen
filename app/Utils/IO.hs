{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.IO (concurrent, withTimeout, debounce) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Utils.Monad ((^>))
import Utils.Time (USec)
import Prelude

concurrent :: IO () -> IO a -> IO a
concurrent thread local = forkIO thread >>= local ^> killThread

withTimeout :: USec -> IO a -> IO (Maybe a)
withTimeout timeout generator = newEmptyMVar >>= waitUntil
  where
    generateJust mvar = generator >>= putMVar mvar . Just
    delayNothing :: MVar (Maybe a) -> IO ()
    delayNothing mvar = threadDelay timeout >> putMVar mvar Nothing
    waitUntil mvar = generateJust mvar `concurrent` delayNothing mvar `concurrent` takeMVar mvar

debounce :: USec -> IO a -> IO a
debounce delay action = action >>= tryMore
  where
    tryMore prev = withTimeout delay action >>= decide prev
    decide prev Nothing = return prev
    decide _ (Just next) = tryMore next
