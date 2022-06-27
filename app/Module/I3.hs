{-# LANGUAGE NoImplicitPrelude #-}

module Module.I3 where

import Config (ItemParams)
import Control.Monad (return)
import Control.Concurrent.MVar (newEmptyMVar, withMVar)
import qualified Lemonbar as L
import Print.I3 (printI3)
import State.I3 (initI3State)
import Update.I3 (initUpdateI3, initUpdateI3Event, updateI3, updateI3Event)
import Utils.Types (Index)
import System.IO (IO)

type Printer = ItemParams -> Index -> L.Powerlemon

type Initializer = IO ()

type Updater = IO ()

buildModule :: [ItemParams] -> IO (Printer, [(Initializer, Updater)])
buildModule params = do
  shared <- initI3State
  private <- newEmptyMVar
  privateEvent <- newEmptyMVar

  let printer = printI3 shared
      init = initUpdateI3 private
      initEvent = initUpdateI3Event privateEvent
      update = updateI3 private shared params
      updateEvent = updateI3Event privateEvent shared params
  return (printer, [(init, update), (initEvent, updateEvent)])
