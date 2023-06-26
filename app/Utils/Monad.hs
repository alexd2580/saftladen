{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Monad ((^>), (>^), concatMapM) where

import Control.Applicative ((<*))
import Control.Monad (Monad, mapM)
import Data.Foldable (concat)
import Data.Function ((.))
import Data.Functor (fmap)
import Data.Traversable (Traversable)

(^>) :: Monad m => m a -> (b' -> m b) -> b' -> m a
(^>) ma bmb = (ma <*) . bmb

(>^) :: Monad m => (a' -> m a) -> m b -> a' -> m a
(>^) ama mb = (<* mb) . ama

concatMapM :: (Traversable t, Monad m) => (a -> m [b]) -> t a -> m [b]
concatMapM f = fmap concat . mapM f
