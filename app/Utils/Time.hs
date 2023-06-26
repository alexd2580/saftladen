{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Time (USec, minutes, seconds, millis) where

import Prelude

type USec = Int

minutes :: Int -> USec
minutes = seconds . (* 60)

seconds :: Int -> USec
seconds = millis . (* 1000)

millis :: Int -> USec
millis = (* 1000)
