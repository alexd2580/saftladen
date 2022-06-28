{-# LANGUAGE NoImplicitPrelude #-}

module Config where

import Control.Monad (return)
import Data.Eq (Eq)
import Data.Function (($))
import qualified Data.Map as Map
import Data.Ord (Ord)
import Data.String (String)
import System.IO (IO)

data ItemType = I3 | Time | Ethernet | Weather | PulseAudio | Load | Storage deriving (Eq, Ord)

type ItemParams = Map.Map String String

data ItemConfig = ItemConfig ItemType ItemParams

data BarConfig = BarConfig [ItemConfig] [ItemConfig] [ItemConfig]

readConfig :: IO BarConfig
readConfig = return $ BarConfig [ItemConfig I3 Map.empty] [] [ItemConfig Time $ Map.fromList [("format", "%F %R")]]
