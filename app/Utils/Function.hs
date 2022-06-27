{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Function where

import Data.Bool (Bool, otherwise)
import Data.Foldable (Foldable, foldr)
import Data.List (map)
import Data.Monoid (Monoid, mempty)

infixl 0 $-

($-) :: (a -> b) -> a -> b
($-) f = f

mapWhere :: (x -> Bool) -> (x -> x) -> [x] -> [x]
mapWhere predicate transform = map forEach
  where
    forEach x
      | predicate x = transform x
      | otherwise = x

fold :: (Monoid m, Foldable f) => (x -> m -> m) -> f x -> m
fold combine = foldr combine mempty
