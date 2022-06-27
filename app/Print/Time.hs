{-# LANGUAGE NoImplicitPrelude #-}

module Print.Time where

import Control.Concurrent.MVar (MVar)
import State.Time as Time
import qualified Lemonbar as L
import Config (ItemParams)
import Utils.Types (Index)
import Data.Function (($))
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Map (findWithDefault)

printTime :: MVar Time.TimeState -> ItemParams -> Index -> L.Powerlemon
printTime shared params monitorIndex = L.withMVar shared $ \(Time.TimeState zonedTime) -> do
  L.setStyle L.Common
  L.openSection L.neutralColorPair
  let timeFormat = findWithDefault "Set 'format' parameter for the time item" "format" params
  L.write $ formatTime defaultTimeLocale timeFormat zonedTime
  L.closeSection
