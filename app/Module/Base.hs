{-# LANGUAGE NoImplicitPrelude #-}

module Module.Base where

import Config (ItemParams)
import Lemonbar (Powerlemon)
import System.IO (IO)
import Utils.Types (Index)

type Printer = ItemParams -> Index -> Powerlemon

type Initializer = IO ()

type Updater = IO ()
