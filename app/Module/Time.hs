{-# LANGUAGE NoImplicitPrelude #-}

module Module.Time where

import Config (ItemParams)
import Control.Monad (return)
import Module.Base (Initializer, Printer, Updater)
import Print.Time (printTime)
import System.IO (IO)
import Utils.Time (USec, seconds)

buildModule :: [ItemParams] -> IO (Printer, [(Initializer, Updater, USec)])
buildModule _ = return (printTime, [(return (), return (), seconds 30)])
