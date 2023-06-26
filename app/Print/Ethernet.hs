{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Print.Ethernet (print) where

import Control.Concurrent.MVar (MVar)
import Lemonbar qualified as L
import Network.Info (NetworkInterface (NetworkInterface))
import State.Ethernet (EthernetState (EthernetState))
import Utils.Color qualified as C
import Utils.MVar (withMVar)
import Utils.Types (Index)
import Prelude

print :: MVar EthernetState -> Index -> L.Powerlemon
print shared _monitorIndex = withMVar shared $ \(EthernetState interfaces) -> do
  L.setStyle L.Common

  forM_ interfaces $ \(NetworkInterface iface v4 _ _, up) -> do
    when ("e" `isPrefixOf` iface) $ do
      L.openSection C.neutralColorPair
      L.write "\63231 "
      L.write iface
      L.write " "
      let sectionColor = if up then C.neutralColorPair else C.warnColorPair
      L.openSection sectionColor
      L.write " "
      L.write $ show v4
      L.write " "
      L.closeSection
