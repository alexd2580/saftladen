{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.List (mapWhere, fold, pop) where

import Utils.Tuple (mapFst)
import Prelude

mapWhere :: (x -> Bool) -> (x -> x) -> [x] -> [x]
mapWhere predicate transform = map forEach
  where
    forEach x
      | predicate x = transform x
      | otherwise = x

fold :: (Monoid m, Foldable f) => (x -> m -> m) -> f x -> m
fold combine = foldr combine mempty

pop :: (a -> Bool) -> [a] -> ([a], Maybe a)
pop _ [] = ([], Nothing)
pop f (x : xs)
  | f x = (xs, Just x)
  | otherwise = mapFst (x :) $ pop f xs
