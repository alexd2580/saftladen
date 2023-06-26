{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module State.Ethernet (EthernetState (EthernetState), init) where

import Control.Concurrent.MVar (MVar, newMVar)
import Network.Info qualified as Net
import Prelude

newtype EthernetState = EthernetState [(Net.NetworkInterface, Bool)] deriving stock (Show)

init :: IO (MVar EthernetState)
init = newMVar $ EthernetState []
