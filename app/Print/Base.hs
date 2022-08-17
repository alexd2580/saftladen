{-# LANGUAGE NoImplicitPrelude #-}

module Print.Base where

import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Data.Functor ((<$>))
import System.IO (IO)

type PingChannel = Chan ()

type PingIO = IO ()
type PongIO = IO ()

createPingChannel :: IO (PongIO, PingIO)
createPingChannel = (\chan -> (readChan chan, writeChan chan ())) <$> newChan
