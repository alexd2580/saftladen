{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.BiMultiMap (BiMultiMap, empty, insert, lookup, moveValue, removeValue) where

import Data.Map.Strict qualified as Map
import Prelude hiding (map)

type Forward k v = Map.Map k [v]

type Reverse k v = Map.Map v k

data BiMultiMap k v = BiMultiMap (Forward k v) (Reverse k v) deriving stock (Show)

type MayTransform x = x -> Either String x

empty :: BiMultiMap k v
empty = BiMultiMap Map.empty Map.empty

insertForward :: Ord k => k -> v -> Forward k v -> Forward k v
insertForward k v = Map.alter (Just . (v :) . fromMaybe []) k

removeForward :: (Ord k, Eq v) => k -> v -> Forward k v -> Forward k v
removeForward k v = Map.adjust (filter (/= v)) k

insertReverse :: Ord v => k -> v -> MayTransform (Reverse k v)
insertReverse k v map =
  if Map.member v map
    then Left "value associated to different key!"
    else Right $ Map.insert v k map

insert :: (Ord k, Ord v) => k -> v -> MayTransform (BiMultiMap k v)
insert k v (BiMultiMap forward reverse) = do
  let newForward = insertForward k v forward
  newReverse <- insertReverse k v reverse
  return $ BiMultiMap newForward newReverse

lookup :: Ord k => k -> BiMultiMap k v -> Maybe [v]
lookup k (BiMultiMap forward _) = Map.lookup k forward

moveValue :: (Ord k, Ord v) => v -> k -> MayTransform (BiMultiMap k v)
moveValue v k (BiMultiMap forward reverse) = do
  let maybePrevKey = Map.lookup v reverse
  prevKey <- maybe (Left "value not mapped!") Right maybePrevKey
  let newForward = insertForward k v $ removeForward prevKey v forward
      newReverse = Map.insert v k reverse
  return $ BiMultiMap newForward newReverse

removeValue :: (Ord k, Ord v) => v -> MayTransform (BiMultiMap k v)
removeValue v (BiMultiMap forward reverse) = do
  let maybePrevKey = Map.lookup v reverse
  prevKey <- maybe (Left "value not mapped!") Right maybePrevKey
  let newForward = removeForward prevKey v forward
      newReverse = Map.delete v reverse
  return $ BiMultiMap newForward newReverse
