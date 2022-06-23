{-# LANGUAGE MagicHash #-}

module TagPartition where

import Data.Foldable (Foldable, foldr)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (Maybe (Just), fromMaybe)
import GHC.Exts (Int (I#), dataToTag#)
import Utils (fold)
import Prelude (Eq, Ord, Show, id, mappend, ($))

newtype ConstructorTag a = ConstructorTag Int deriving (Eq, Ord, Show)

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
