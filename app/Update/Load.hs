{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Update.Load (forkUpdater) where

import Base (PingIO)
import Control.Concurrent (MVar, ThreadId, forkIO, threadDelay)
import Control.Monad (filterM)
import Data.List (words)
import State.Load (DeviceType (DeviceCPU, DeviceMB, DeviceNVME, DeviceUnknown), FreeRam, LoadState (LoadState), Sensor (Sensor, sensorDeviceType, sensorDriverName, sensorFiles, sensorLabel, sensorMonitorName, sensorPrefix, sensorSensorType, sensorValue), SensorType (SensorFan, SensorTemp), TotalRam)
import System.Directory (listDirectory)
import System.IO (FilePath, Handle, IOMode (ReadMode), hGetLine, hIsEOF, withFile)
import Utils.MVar (recomputeState_)
import Utils.Monad (concatMapM)
import Utils.Time (seconds)
import Utils.Types (Name)
import Prelude

readByLine :: Handle -> IO [[String]]
readByLine h = do
  isEof <- hIsEOF h
  if isEof
    then return []
    else do
      line <- hGetLine h
      (words line :) <$> readByLine h

computeRam :: [[String]] -> (TotalRam, FreeRam)
computeRam [] = (0, 0)
computeRam ([param, (* 1024) . read -> value, "kB"] : (computeRam -> ram@(total, free))) = case param of
  "MemTotal:" -> (value, free)
  "MemFree:" -> (total, free + value)
  "Buffers:" -> (total, free + value)
  "Cached:" -> (total, free + value)
  "SReclaimable:" -> (total, free + value)
  _ -> ram
computeRam (_ : xs) = computeRam xs

readFileWords :: FilePath -> IO [[String]]
readFileWords file = withFile file ReadMode readByLine

readFile :: FilePath -> IO String
readFile file = withFile file ReadMode hGetLine

readMemInfo :: IO (TotalRam, FreeRam)
readMemInfo = computeRam <$> readFileWords "/proc/meminfo"

readProcInfo :: IO Float
readProcInfo = read . head . head <$> readFileWords "/proc/loadavg"

sensorInputFilePath :: Sensor -> FilePath
sensorInputFilePath (Sensor hwmon _ prefix _ _ _ _ _) = "/sys/class/hwmon/" ++ hwmon ++ "/" ++ prefix ++ "_input"

inferDeviceType :: Sensor -> Sensor
-- For k10temp we want "Tctl".
inferDeviceType sensor@(Sensor _ "k10temp" _ "Tctl" _ _ _ _) = sensor {sensorDeviceType = DeviceCPU}
-- Ignore anything that is not "Composite".
inferDeviceType sensor@(Sensor _ "nvme" _ "Composite" _ _ _ _) = sensor {sensorDeviceType = DeviceNVME}
{-
nct6797:
CPUTIN -- CPU Socket temp.
AUXTIN0
AUXTIN1
AUXTIN2
AUXTIN3
SMBUSMASTER 0 -- Mirror of CPU Temp.
PCH_CHIP_CPU_MAX_TEMP
PCH_CHIP_TEMP
PCH_CPU_TEMP
TSI0_TEMP -- Mirror of CPU Temp.
-}
inferDeviceType sensor@(Sensor _ "nct6797" _ "SYSTIN" _ _ _ _) = sensor {sensorDeviceType = DeviceMB}
-- Mark rest as unknown.
inferDeviceType sensor = sensor {sensorDeviceType = DeviceUnknown}

partitionSensors :: [FilePath] -> [Sensor]
partitionSensors = match . sort
  where
    match :: [FilePath] -> [Sensor]
    match [] = []
    match xs@(('f' : 'a' : 'n' : (matchIndex -> Just index)) : _) = collect ("fan" ++ index) SensorFan xs
    match xs@(('t' : 'e' : 'm' : 'p' : (matchIndex -> Just index)) : _) = collect ("temp" ++ index) SensorTemp xs
    match (_ : xs) = match xs
    matchIndex :: String -> Maybe String
    matchIndex (d1 : '_' : _) = Just [d1]
    matchIndex (d1 : d2 : '_' : _) = Just [d1, d2]
    matchIndex _ = Nothing

    collect :: String -> SensorType -> [FilePath] -> [Sensor]
    collect prefix sensorType xs = Sensor undefined undefined prefix undefined sensorType undefined yes undefined : match no
      where
        (yes, no) = span ((prefix ++ "_") `isPrefixOf`) xs

filterSensors :: [Sensor] -> IO [Sensor]
filterSensors = filterM isActiveSensor
  where
    isBetween :: Ord a => a -> a -> a -> Bool
    isBetween a b x = a <= x && x <= b

    isActiveSensor :: Sensor -> IO Bool
    isActiveSensor Sensor {sensorDeviceType = DeviceUnknown} = return False
    isActiveSensor
      Sensor
        { sensorMonitorName = hwmonName,
          sensorSensorType = SensorTemp,
          sensorFiles = (find ("_input" `isSuffixOf`) -> Just inputFile)
        } = readFile ("/sys/class/hwmon/" ++ hwmonName ++ '/' : inputFile) <&> read <&> isBetween (5000 :: Int) 100000
    isActiveSensor _ = return False

hasLabel :: Sensor -> Bool
hasLabel = any ("_label" `isSuffixOf`) . sensorFiles

attachLabel :: Sensor -> IO Sensor
attachLabel sensor@Sensor {sensorMonitorName = hwmonName, sensorPrefix = prefix} = do
  label <- readFile ("/sys/class/hwmon/" ++ hwmonName ++ '/' : prefix ++ "_label")
  return sensor {sensorLabel = label}

readHwmon :: FilePath -> IO [Sensor]
readHwmon hwmon =
  listDirectory hwmonDir
    <&> partitionSensors
    <&> filter hasLabel
    <&> map (\sensor -> sensor {sensorMonitorName = hwmon})
    >>= mapM attachLabel
    >>= attachDriverName
    <&> map inferDeviceType
    >>= filterSensors
  where
    hwmonDir = "/sys/class/hwmon/" ++ hwmon
    attachDriverName :: [Sensor] -> IO [Sensor]
    attachDriverName sensors = do
      name <- readFile (hwmonDir ++ "/name")
      return $ map (attachDriverName_ name) sensors
    attachDriverName_ :: Name -> Sensor -> Sensor
    attachDriverName_ name sensor = sensor {sensorMonitorName = hwmon, sensorDriverName = name}

initSensors :: IO [Sensor]
initSensors = listDirectory "/sys/class/hwmon" >>= concatMapM readHwmon

readSensorValue :: Sensor -> IO Sensor
readSensorValue sensor = do
  value <- readFile (sensorInputFilePath sensor) <&> read
  return sensor {sensorValue = value}

forkUpdater :: PingIO -> MVar LoadState -> IO ThreadId
forkUpdater ping sharedState = forkIO $ do
  sensors <- initSensors
  forever $ do
    recomputeState_ sharedState $ do
      sensorsWithValues <- mapM readSensorValue sensors
      (totalRam, freeRam) <- readMemInfo
      load <- readProcInfo

      return $ LoadState load totalRam freeRam sensorsWithValues
    ping
    threadDelay $ seconds 5
