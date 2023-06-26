{-# LANGUAGE Safe #-}

module Prelude
  ( -- Control.Applicative
    (<$>),
    (<**>),
    (<|>),
    -- Control.Monad
    Monad (return, (>>), (>>=)),
    forM,
    forM_,
    forever,
    mapM,
    mapM_,
    msum,
    unless,
    when,
    -- Data.Bool
    Bool (False, True),
    otherwise,
    (&&),
    (||),
    -- Data.Char
    Char,
    chr,
    ord,
    toLower,
    -- Data.Either
    Either (Left, Right),
    either,
    -- Data.Eq
    Eq ((/=), (==)),
    -- Data.Foldable
    Foldable (foldl, foldl1, foldr, foldr1),
    any,
    concatMap,
    -- Data.Function
    const,
    flip,
    id,
    ($),
    (.),
    -- Data.Functor
    fmap,
    void,
    (<&>),
    -- Data.Int
    Int,
    -- Data.List
    concat,
    drop,
    elemIndex,
    filter,
    find,
    head,
    isPrefixOf,
    isSuffixOf,
    length,
    map,
    sort,
    span,
    take,
    unzip,
    zip,
    (!!),
    (++),
    -- Data.Maybe
    Maybe (Just, Nothing),
    fromJust,
    fromMaybe,
    maybe,
    -- Data.Monoid
    Monoid (mappend, mempty),
    -- Data.Ord
    Ord ((<), (<=), (>), (>=)),
    -- Data.Semigroup
    Semigroup ((<>)),
    -- Data.String
    String,
    -- Data.Tuple
    fst,
    snd,
    -- GHC.Err
    error,
    -- GHC.Float
    Float,
    -- GHC.Num
    Num (abs, (*), (+), (-)),
    -- import GHC.Real
    Fractional (fromRational, (/)),
    Integral (div, mod),
    RealFrac (ceiling, floor, round, truncate),
    fromIntegral,
    -- GHC.Show
    Show (show),
    -- System.IO
    IO,
    -- Text.Read
    read,
    -- Prelude
    invalidArguments,
    undefined,
    eitherToError,
  )
where

import Control.Applicative ((<$>), (<**>), (<|>))
import Control.Monad
  ( Monad (return, (>>), (>>=)),
    forM,
    forM_,
    forever,
    mapM,
    mapM_,
    msum,
    unless,
    when,
  )
import Data.Bool (Bool (False, True), otherwise, (&&), (||))
import Data.Char (Char, chr, ord, toLower)
import Data.Either (Either (Left, Right), either)
import Data.Eq (Eq ((/=), (==)))
import Data.Foldable (Foldable (foldl, foldl1, foldr, foldr1), any, concatMap)
import Data.Function (const, flip, id, ($), (.))
import Data.Functor (fmap, void, (<&>))
import Data.Int (Int)
import Data.List
  ( concat,
    drop,
    elemIndex,
    filter,
    find,
    head,
    isPrefixOf,
    isSuffixOf,
    length,
    map,
    sort,
    span,
    take,
    unzip,
    zip,
    (!!),
    (++),
  )
import Data.Maybe (Maybe (Just, Nothing), fromJust, fromMaybe, maybe)
import Data.Monoid (Monoid (mappend, mempty))
import Data.Ord (Ord ((<), (<=), (>), (>=)))
import Data.Semigroup (Semigroup ((<>)))
import Data.String (String)
import Data.Tuple (fst, snd)
import GHC.Err (error)
import GHC.Float (Float)
import GHC.Num (Num (abs, (*), (+), (-)))
import GHC.Real (Fractional (fromRational, (/)), Integral (div, mod), RealFrac (ceiling, floor, round, truncate), fromIntegral)
import GHC.Show (Show (show))
import System.IO (IO)
import Text.Read (read)

invalidArguments :: Show a => String -> a -> e
invalidArguments name a = error $ "Invalid arguments to " ++ name ++ ": " ++ show a

undefined :: a
undefined = error "undefined"

eitherToError :: Either String a -> a
eitherToError = either error id
