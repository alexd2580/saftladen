{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module State.Load
  ( Load,
    TotalRam,
    FreeRam,
    MonitorName,
    DriverName,
    Label,
    SensorType
      ( SensorTemp,
        SensorFan
      ),
    DeviceType
      ( DeviceCPU,
        DeviceGPU,
        DeviceHDD,
        DeviceSSD,
        DeviceNVME,
        DeviceMB,
        DeviceUnknown
      ),
    Sensor
      ( Sensor,
        sensorMonitorName,
        sensorDriverName,
        sensorPrefix,
        sensorLabel,
        sensorSensorType,
        sensorDeviceType,
        sensorFiles,
        sensorValue
      ),
    LoadState (LoadState),
    init,
  )
where

import Control.Concurrent.MVar (MVar, newMVar)
import System.IO (FilePath)
import Utils.Types (Name)
import Prelude

type Load = Float

type TotalRam = Int

type FreeRam = Int

type MonitorName = Name

type DriverName = Name

type Label = Name

data SensorType = SensorTemp | SensorFan deriving stock (Show)

data DeviceType = DeviceCPU | DeviceGPU | DeviceHDD | DeviceSSD | DeviceNVME | DeviceMB | DeviceUnknown deriving stock (Show)

data Sensor = Sensor
  { sensorMonitorName :: MonitorName,
    sensorDriverName :: DriverName,
    sensorPrefix :: String,
    sensorLabel :: Label,
    sensorSensorType :: SensorType,
    sensorDeviceType :: DeviceType,
    sensorFiles :: [FilePath],
    sensorValue :: Int
  }
  deriving stock (Show)

data LoadState = LoadState Load TotalRam FreeRam [Sensor] deriving stock (Show)

init :: IO (MVar LoadState)
init = newMVar $ LoadState 0 0 0 []
