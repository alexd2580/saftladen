{-# LANGUAGE NoImplicitPrelude #-}

module Update.Ethernet where

import System.IO (IO)
import Config (ItemConfig)
import qualified Network.Info as Net
import State.Ethernet (EthernetMState, EthernetState(EthernetState))
import Utils.MVar(modifyState_)
import Data.Bool(Bool(False))
import Data.Function (($), const)
import Data.List (zip)

updateEthernet :: [ItemConfig] -> EthernetMState -> IO ()
updateEthernet configs sharedState = do
  interfaces <- Net.getNetworkInterfaces
  let connectivity = [False, False, False, False, False, False, False, False]
  modifyState_ sharedState $ const $ EthernetState $ zip interfaces connectivity
