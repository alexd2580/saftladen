{-# LANGUAGE NoImplicitPrelude #-}

module Update.Time where

import Config (ItemConfig)
import Control.Monad ((>>=))
import Data.Function (const, (.))
import Data.Time.LocalTime (getZonedTime)
import State.Time (TimeMState, TimeState (TimeState))
import System.IO (IO)
import Utils.MVar (modifyState_)

updateTime :: [ItemConfig] -> TimeMState -> IO ()
updateTime _ sharedState = getZonedTime >>= modifyState_ sharedState . const . TimeState
