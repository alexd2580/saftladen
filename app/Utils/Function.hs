{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Function (($-), (.-), then_, cond, guard) where

import Prelude

infixl 0 $-

($-) :: (a -> b) -> a -> b
($-) f = f

(.-) :: (a -> b) -> (b -> c) -> a -> c
(.-) = flip (.)

then_ :: Monad m => (a -> m b) -> (b -> c) -> a -> m c
then_ amb bc a = amb a <&> bc

cond :: (a -> Bool) -> (a -> a) -> (a -> a) -> a -> a
cond pred ifF elseF value
  | pred value = ifF value
  | otherwise = elseF value

guard :: (a -> Bool) -> (a -> a) -> a -> a
guard pred transform = cond pred transform id
