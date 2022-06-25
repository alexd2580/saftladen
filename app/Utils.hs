{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils where

import Control.Concurrent (forkIO, readChan, threadDelay)
import Control.Concurrent.Chan (Chan)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forever, return, void, (>>), (>>=))
import Data.Bool (Bool (True), otherwise)
import Data.Foldable (Foldable, foldr)
import Data.Function (($), (.))
import Data.Int (Int)
import Data.List (map)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Monoid (Monoid, mempty)
import System.IO (IO)
import Prelude ((*))

infixl 0 $-

($-) :: (a -> b) -> a -> b
($-) = ($)

mapWhere :: (x -> Bool) -> (x -> x) -> [x] -> [x]
mapWhere predicate transform = map forEach
  where
    forEach x
      | predicate x = transform x
      | otherwise = x

type USec = Int

seconds :: Int -> USec
seconds = (* 1000000)

millis :: Int -> USec
millis = (* 1000)

forkForever :: IO a -> IO ()
forkForever = void . forkIO . forever

fold :: (Monoid m, Foldable f) => (x -> m -> m) -> f x -> m
fold combine = foldr combine mempty

withTimeout :: USec -> IO a -> IO (Maybe a)
withTimeout timeout generator = newEmptyMVar >>= waitUntil
  where
    generateJust mvar = generator >>= putMVar mvar . Just
    delayNothing mvar = threadDelay timeout >> putMVar mvar Nothing
    populate mvar = forkIO (generateJust mvar) >> delayNothing mvar
    waitUntil mvar = forkIO (populate mvar) >> takeMVar mvar

debounce :: USec -> IO a -> IO a
debounce delay action = action >>= tryMore
  where
    tryMore prev = withTimeout delay action >>= decide prev
    decide prev Nothing = return prev
    decide _ (Just next) = tryMore next
