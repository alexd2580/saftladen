{-# LANGUAGE NoImplicitPrelude #-}

module State.Time where

import Control.Concurrent.MVar (MVar, newMVar)
import Control.Monad ((>>=))
import Data.Function ((.))
import Data.Time.LocalTime (ZonedTime, getZonedTime)
import System.IO (IO)
import Text.Show (Show)

newtype TimeState = TimeState ZonedTime deriving (Show)

type TimeMState = MVar TimeState

initTimeState :: IO TimeMState
initTimeState = getZonedTime >>= newMVar . TimeState
