{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Update.Weather (queryWeather, forkUpdater) where

import Base (PingIO)
import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Concurrent.MVar (MVar)
import Data.Aeson (FromJSON (parseJSON), Options (fieldLabelModifier), defaultOptions, genericParseJSON)
import Data.Time (TimeOfDay (TimeOfDay, todHour, todMin, todSec))
import GHC.Generics (Generic)
import Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest)
import State.Weather
  ( WeatherState
      ( NoWeatherState,
        WeatherState,
        weatherFeelsLike,
        weatherHumidity,
        weatherMaxTemp,
        weatherMinTemp,
        weatherSunrise,
        weatherSunset,
        weatherTemp,
        weatherWeatherCode,
        weatherWeatherDesc
      ),
  )
import Utils.MVar (recomputeState_)
import Utils.Time (minutes)
import Prelude

lowercase :: String -> String
lowercase [] = []
lowercase (x : xs) = toLower x : xs

newtype WeatherDesc = WeatherDesc
  { weatherDescValue :: String
  }
  deriving stock (Generic)

weatherDescNames :: String -> String
weatherDescNames = lowercase . drop 11

instance FromJSON WeatherDesc where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = weatherDescNames}

data CurrentCondition = CurrentCondition
  { currentConditionTemp :: String,
    currentConditionFeelsLike :: String,
    currentConditionHumidity :: String,
    currentConditionWeatherCode :: String,
    currentConditionWeatherDesc :: [WeatherDesc]
  }
  deriving stock (Generic)

currentConditionNames :: String -> String
currentConditionNames field = case lowercase $ drop 16 field of
  "temp" -> "temp_C"
  "feelsLike" -> "FeelsLikeC"
  x -> x

instance FromJSON CurrentCondition where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = currentConditionNames}

data Astronomy = Astronomy
  { astronomySunrise :: String,
    astronomySunset :: String
  }
  deriving stock (Generic)

astronomyNames :: String -> String
astronomyNames = lowercase . drop 9

instance FromJSON Astronomy where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = astronomyNames}

data Forecast = Forecast
  { forecastAstronomy :: [Astronomy],
    forecastMinTemp :: String,
    forecastMaxTemp :: String
  }
  deriving stock (Generic)

forecastNames :: String -> String
forecastNames field = case lowercase $ drop 8 field of
  "minTemp" -> "mintempC"
  "maxTemp" -> "maxtempC"
  x -> x

instance FromJSON Forecast where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = forecastNames}

data WttrInResponse = WttrInResponse
  { wttrInCurrentCondition :: [CurrentCondition],
    wttrInWeather :: [Forecast]
  }
  deriving stock (Generic)

wttrInResponseNames :: String -> String
wttrInResponseNames field = case lowercase $ drop 6 field of
  "currentCondition" -> "current_condition"
  x -> x

instance FromJSON WttrInResponse where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = wttrInResponseNames}

-- {
--     "current_condition": [
--         {
--             "FeelsLikeC": "20",
--             "FeelsLikeF": "68",
--             "cloudcover": "75",
--             "humidity": "64",
--             "localObsDateTime": "2022-08-29 04:12 PM",
--             "observation_time": "02:12 PM",
--             "precipInches": "0.0",
--             "precipMM": "0.0",
--             "pressure": "1021",
--             "pressureInches": "30",
--             "temp_C": "20",
--             "temp_F": "68",
--             "uvIndex": "5",
--             "visibility": "10",
--             "visibilityMiles": "6",
--             "weatherCode": "116",
--             "weatherDesc": [
--                 {
--                     "value": "Partly cloudy"
--                 }
--             ],
--             "weatherIconUrl": [
--                 {
--                     "value": ""
--                 }
--             ],
--             "winddir16Point": "NNW",
--             "winddirDegree": "340",
--             "windspeedKmph": "15",
--             "windspeedMiles": "9"
--         }
--     ],
--     "nearest_area": [
--         {
--             "areaName": [
--                 {
--                     "value": "Westmoor"
--                 }
--             ],
--             "country": [
--                 {
--                     "value": "Germany"
--                 }
--             ],
--             "latitude": "53.483",
--             "longitude": "9.683",
--             "population": "0",
--             "region": [
--                 {
--                     "value": "Niedersachsen"
--                 }
--             ],
--             "weatherUrl": [
--                 {
--                     "value": ""
--                 }
--             ]
--         }
--     ],
--     "request": [
--         {
--             "query": "Lat 53.48 and Lon 9.70",
--             "type": "LatLon"
--         }
--     ],
--     "weather": [
--         {
--             "astronomy": [
--                 {
--                     "moon_illumination": "11",
--                     "moon_phase": "Waxing Crescent",
--                     "moonrise": "08:29 AM",
--                     "moonset": "09:18 PM",
--                     "sunrise": "06:25 AM",
--                     "sunset": "08:18 PM"
--                 }
--             ],
--             "avgtempC": "18",
--             "avgtempF": "64",
--             "date": "2022-08-29",
--             "maxtempC": "22",
--             "maxtempF": "72",
--             "mintempC": "14",
--             "mintempF": "56",
--             "sunHour": "11.5",
--             "totalSnow_cm": "0.0",
--             "uvIndex": "4"
--         },
--         {
--             "astronomy": [
--                 {
--                     "moon_illumination": "19",
--                     "moon_phase": "Waxing Crescent",
--                     "moonrise": "09:47 AM",
--                     "moonset": "09:28 PM",
--                     "sunrise": "06:26 AM",
--                     "sunset": "08:16 PM"
--                 }
--             ],
--             "avgtempC": "17",
--             "avgtempF": "63",
--             "date": "2022-08-30",
--             "maxtempC": "24",
--             "maxtempF": "74",
--             "mintempC": "12",
--             "mintempF": "54",
--             "sunHour": "12.5",
--             "totalSnow_cm": "0.0",
--             "uvIndex": "4"
--         },
--         {
--             "astronomy": [
--                 {
--                     "moon_illumination": "22",
--                     "moon_phase": "Waxing Crescent",
--                     "moonrise": "11:07 AM",
--                     "moonset": "09:40 PM",
--                     "sunrise": "06:28 AM",
--                     "sunset": "08:13 PM"
--                 }
--             ],
--             "avgtempC": "17",
--             "avgtempF": "62",
--             "date": "2022-08-31",
--             "maxtempC": "24",
--             "maxtempF": "76",
--             "mintempC": "10",
--             "mintempF": "51",
--             "sunHour": "13.5",
--             "totalSnow_cm": "0.0",
--             "uvIndex": "5"
--         }
--     ]
-- }

parseInt :: String -> Int
parseInt = read

parseTime :: String -> TimeOfDay
parseTime [h1, h2, ':', m1, m2, ' ', aorp, 'M'] =
  TimeOfDay
    { todHour = read [h1, h2] + if aorp == 'P' then 12 else 0,
      todMin = read [m1, m2],
      todSec = 0
    }
parseTime x = error $ "Can't parse time" ++ show x

queryWeather :: IO WttrInResponse
queryWeather = do
  req <- parseRequest "GET https://wttr.in?format=j2"
  getResponseBody <$> httpJSON req

parseWeather :: WttrInResponse -> WeatherState
parseWeather
  ( WttrInResponse
      [ CurrentCondition
          (parseInt -> temp)
          (parseInt -> feelsLike)
          (parseInt -> humidity)
          (parseInt -> weatherCode)
          [WeatherDesc weatherDesc]
        ]
      ( ( Forecast
            [ Astronomy
                (parseTime -> sunrise)
                (parseTime -> sunset)
              ]
            (parseInt -> minTemp)
            (parseInt -> maxTemp)
          )
          : _
        )
    ) =
    WeatherState
      { weatherTemp = temp,
        weatherFeelsLike = feelsLike,
        weatherMinTemp = minTemp,
        weatherMaxTemp = maxTemp,
        weatherHumidity = humidity,
        weatherWeatherCode = weatherCode,
        weatherWeatherDesc = weatherDesc,
        weatherSunrise = sunrise,
        weatherSunset = sunset
      }
parseWeather _ = NoWeatherState

forkUpdater :: PingIO -> MVar WeatherState -> IO ThreadId
forkUpdater ping sharedState = forkIO $
  forever $ do
    recomputeState_ sharedState $ queryWeather <&> parseWeather
    ping
    threadDelay $ minutes 30
