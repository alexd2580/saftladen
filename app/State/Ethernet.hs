{-# LANGUAGE NoImplicitPrelude #-}

module State.Ethernet where

import Control.Concurrent.MVar (MVar, newMVar)
import Data.Bool (Bool)
import Data.Function (($))
import qualified Network.Info as Net
import System.IO (IO)
import Text.Show (Show)

newtype EthernetState = EthernetState [(Net.NetworkInterface, Bool)] deriving (Show)

type EthernetMState = MVar EthernetState

initEthernetState :: IO EthernetMState
initEthernetState = newMVar $ EthernetState []
