{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Time where

import Data.Int (Int)
import Prelude ((*))

type USec = Int

seconds :: Int -> USec
seconds = (* 1000000)

millis :: Int -> USec
millis = (* 1000)
