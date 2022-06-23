module Config where

import Data.Default (Default (def))
import Utils (USec, seconds)

-- I3 Config
data I3Config = I3Config

instance Default I3Config where
  def = I3Config

-- Time Config
type TimeFormat = String

newtype TimeConfig = TimeConfig TimeFormat

instance Default TimeConfig where
  def = TimeConfig "%F %R"

-- Weather Config
data WeatherConfig = WeatherConfig

instance Default WeatherConfig where
  def = WeatherConfig

-- PulseAudio Config
data PulseAudioConfig = PulseAudioConfig

instance Default PulseAudioConfig where
  def = PulseAudioConfig

-- Network Config
type Interface = String

newtype NetworkConfig = NetworkConfig Interface

instance Default NetworkConfig where
  def = NetworkConfig "auto-wifi"

-- Load Config
data LoadConfig = LoadConfig

instance Default LoadConfig where
  def = LoadConfig

-- Storage Config
data StorageConfig = StorageConfig

instance Default StorageConfig where
  def = StorageConfig

data ItemContentConfig
  = I3 I3Config
  | Time TimeConfig
  | Weather WeatherConfig
  | PulseAudio PulseAudioConfig
  | Network NetworkConfig
  | Load LoadConfig
  | Storage StorageConfig

data ItemConfig = ItemConfig USec Bool ItemContentConfig

data BarConfig = BarConfig  [ItemConfig] [ItemConfig] [ItemConfig]

readConfig :: IO BarConfig
readConfig = return $ BarConfig [defaultItemConfig $ I3 def] [] [defaultItemConfig $ Time def]
  where defaultItemConfig = ItemConfig (seconds 30) True
