{-# LANGUAGE NoImplicitPrelude #-}

module Print.Time where

import Control.Monad.Trans (lift)
import Data.Function (($))
import Data.Map (findWithDefault)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime)
import qualified Lemonbar as L
import Module.Base (Printer)

printTime :: Printer
printTime params monitorIndex = do
  zonedTime <- lift getZonedTime
  L.setStyle L.Common
  L.openSection L.neutralColorPair
  let timeFormat = findWithDefault "Set 'format' parameter for the time item" "format" params
  L.write $ formatTime defaultTimeLocale timeFormat zonedTime
  L.closeSection
