{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Tuple (uncurry3, map4, zip4, zipWith4, mapFst, mapSnd) where

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

map4 :: (a -> b) -> (a, a, a, a) -> (b, b, b, b)
map4 f (a, b, c, d) = (f a, f b, f c, f d)

zip4 :: (a, a, a, a) -> (b, b, b, b) -> ((a, b), (a, b), (a, b), (a, b))
zip4 (a1, b1, c1, d1) (a2, b2, c2, d2) = ((a1, a2), (b1, b2), (c1, c2), (d1, d2))

zipWith4 :: (a -> b -> c) -> (a, a, a, a) -> (b, b, b, b) -> (c, c, c, c)
zipWith4 f (a1, b1, c1, d1) (a2, b2, c2, d2) = (f a1 a2, f b1 b2, f c1 c2, f d1 d2)

mapFst :: (a -> a') -> (a, b) -> (a', b)
mapFst f (a, b) = (f a, b)

mapSnd :: (b -> b') -> (a, b) -> (a, b')
mapSnd f (a, b) = (a, f b)
