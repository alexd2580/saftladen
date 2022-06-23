module Utils where

import Control.Concurrent (forkIO)
import Control.Monad (forever, void)
import Prelude (mempty, Int, IO, Monoid, (*), (.))
import Data.Foldable (Foldable, foldr)

type USec = Int

seconds :: Int -> USec
seconds = (* 1000000)

forkForever :: IO a -> IO ()
forkForever = void . forkIO . forever

fold :: (Monoid m, Foldable f) => (x -> m -> m) -> f x -> m
fold combine = foldr combine mempty

-- withTimeout :: USec -> IO a -> IO (Maybe a)
-- withTimeout timeout generator = do
--   mvar <- newEmptyMVar
--   forkIO $ do
--     forkIO $ do
--       result <- generator
--       putMVar mvar $ Just result
--
--     threadDelay timeout
--     putMVar mvar Nothing
--
--   takeMVar mvar
