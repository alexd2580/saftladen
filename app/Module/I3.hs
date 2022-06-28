{-# LANGUAGE NoImplicitPrelude #-}

module Module.I3 where

import Config (ItemParams)
import Control.Concurrent.MVar (newEmptyMVar)
import Control.Monad (return)
import Module.Base (Initializer, Printer, Updater)
import Print.I3 (printI3)
import State.I3 (initI3State)
import System.IO (IO)
import Update.I3 (initUpdateI3, initUpdateI3Event, updateI3, updateI3Event)
import Utils.Time (USec, seconds)

buildModule :: [ItemParams] -> IO (Printer, [(Initializer, Updater, USec)])
buildModule params = do
  shared <- initI3State
  private <- newEmptyMVar
  privateEvent <- newEmptyMVar

  let printer = printI3 shared
      init = initUpdateI3 private
      initEvent = initUpdateI3Event privateEvent
      update = updateI3 private shared params
      updateEvent = updateI3Event privateEvent shared params
  return (printer, [(init, update, seconds 300), (initEvent, updateEvent, seconds 0)])
