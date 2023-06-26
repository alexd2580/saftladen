{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Print.Time (print) where

import Control.Monad.Trans (lift)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime)
import Lemonbar qualified as L
import Utils.Color qualified as C
import Prelude

print :: Int -> L.Powerlemon
print _monitorIndex = do
  zonedTime <- lift getZonedTime
  L.setStyle L.Common
  L.openSection C.focusedColorPair
  L.write "\61463 "
  L.write $ formatTime defaultTimeLocale "%F %R" zonedTime
  L.write " "
  L.closeSection
