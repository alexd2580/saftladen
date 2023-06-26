{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.TagPartition (getTag, partitionByConstructorTagWith, partitionByConstructorTag) where

import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Exts (dataToTag#)
import GHC.Int (Int (I#))
import Utils.Function (fold)
import Prelude

newtype ConstructorTag a = ConstructorTag Int deriving stock (Eq, Ord, Show)

getTag :: a -> ConstructorTag a
getTag a = ConstructorTag $ I# (dataToTag# a)

partitionByConstructorTagWith :: Foldable f => (a -> b) -> f a -> Map (ConstructorTag b) [a]
partitionByConstructorTagWith transform = fold insertByTag
  where
    -- Something wrong with forall b/b1...
    -- insertByTag :: a -> Map (ConstructorTag b) [a] -> Map (ConstructorTag b) [a]
    insertByTag a = Map.alter (mappend $ Just [a]) $ getTag $ transform a

partitionByConstructorTag :: Foldable f => f a -> Map (ConstructorTag a) [a]
partitionByConstructorTag = partitionByConstructorTagWith id
