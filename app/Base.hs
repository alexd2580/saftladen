{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Base
  ( PingChannel,
    PingIO,
    PongIO,
    createPingChannel,
  )
where

import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Prelude (IO, (<$>))

type PingChannel = Chan ()

type PingIO = IO ()

type PongIO = IO ()

createPingChannel :: IO (PongIO, PingIO)
createPingChannel = (\chan -> (readChan chan, writeChan chan ())) <$> newChan
