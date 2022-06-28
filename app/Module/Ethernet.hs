{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Module.Ethernet where

import Config (ItemConfig, ItemParams)
import Control.Applicative ((<$>))
import Control.Concurrent.MVar (MVar, newEmptyMVar, withMVar)
import Control.Monad (return)
import Data.Bool (Bool (False))
import Data.Function (const, ($), (.))
import Data.List (map, zip)
import qualified Lemonbar as L
import Module.Base (Initializer, Printer, Updater)
import qualified Network.Info as Net
import Print.Ethernet (printEthernet)
import State.Ethernet (EthernetState (EthernetState), initEthernetState)
import System.IO (IO)
import Utils.MVar (modifyState_, recomputeState_)
import Utils.Time (USec, seconds)
import Utils.Types (Index)

updateEthernet :: MVar EthernetState -> [ItemParams] -> IO ()
updateEthernet sharedState _ = recomputeState_ sharedState $ EthernetState . map (,False) <$> Net.getNetworkInterfaces

buildModule :: [ItemParams] -> IO (Printer, [(Initializer, Updater, USec)])
buildModule params = do
  shared <- initEthernetState

  let printer = printEthernet shared
      update = updateEthernet shared params
  return (printer, [(return (), update, seconds 60)])
