{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Update.Ethernet (forkUpdater) where

import Base (PingIO)
import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Concurrent.MVar (MVar)
import Network.Info qualified as Net
import State.Ethernet (EthernetState (EthernetState))
import Utils.MVar (recomputeState_)
import Utils.Time (seconds)
import Prelude

forkUpdater :: PingIO -> MVar EthernetState -> IO ThreadId
forkUpdater ping sharedState = forkIO $
  forever $ do
    recomputeState_ sharedState $ EthernetState . map (,False) <$> Net.getNetworkInterfaces
    ping
    threadDelay $ seconds 5
