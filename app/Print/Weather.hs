{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Print.Weather (print) where

import Control.Concurrent.MVar (MVar)
import Control.Monad.Trans (lift)
import Data.Time (DiffTime, LocalTime (localTimeOfDay), TimeOfDay (TimeOfDay, todHour, todMin), ZonedTime (zonedTimeToLocalTime), getZonedTime, secondsToDiffTime, timeOfDayToTime, timeToTimeOfDay)
import Lemonbar qualified as L
import State.Weather (WeatherState (NoWeatherState, WeatherState))
import Utils.MVar (withMVar)
import Utils.Types (Index)
import Utils.Color qualified as C
import Prelude

weatherIcon :: Int -> String
-- nf-weather-thunderstorm
weatherIcon 389 = "\xe31d" -- 389 Moderate or heavy rain in area with thunder
weatherIcon 386 = "\xe31d" -- 386 Patchy light rain in area with thunder
weatherIcon 200 = "\xe31d" -- 200 Thundery outbreaks in nearby

-- nf-weather-showers
weatherIcon 266 = "\xe319" -- 266 Light drizzle
weatherIcon 263 = "\xe319" -- 263 Patchy light drizzle
weatherIcon 293 = "\xe319" -- 293 Patchy light rain
weatherIcon 176 = "\xe319" -- 176 Patchy rain nearby
weatherIcon 296 = "\xe319" -- 296 Light rain
weatherIcon 353 = "\xe319" -- 353 Light rain shower

-- nf-weather-rain
weatherIcon 302 = "\xe318" -- 302 Moderate rain
weatherIcon 299 = "\xe318" -- 299 Moderate rain at times
weatherIcon 356 = "\xe318" -- 356 Moderate or heavy rain shower
weatherIcon 308 = "\xe318" -- 308 Heavy rain
weatherIcon 305 = "\xe318" -- 305 Heavy rain at times
weatherIcon 359 = "\xe318" -- 359 Torrential rain shower

-- nf-weather-snow
weatherIcon 179 = "\xe31a" -- 179 Patchy snow nearby
weatherIcon 323 = "\xe31a" -- 323 Patchy light snow
weatherIcon 326 = "\xe31a" -- 326 Light snow
weatherIcon 368 = "\xe31a" -- 368 Light snow showers

-- nf-weather-snow_wind
weatherIcon 395 = "\xe35e" -- 395 Moderate or heavy snow in area with thunder
weatherIcon 392 = "\xe35e" -- 392 Patchy light snow in area with thunder
weatherIcon 329 = "\xe35e" -- 329 Patchy moderate snow
weatherIcon 332 = "\xe35e" -- 332 Moderate snow
weatherIcon 338 = "\xe35e" -- 338 Heavy snow
weatherIcon 371 = "\xe35e" -- 371 Moderate or heavy snow showers
weatherIcon 335 = "\xe35e" -- 335 Patchy heavy snow
weatherIcon 227 = "\xe35e" -- 227 Blowing snow
weatherIcon 230 = "\xe35e" -- 230 Blizzard

-- nf-weather-sleet
weatherIcon 365 = "\xe3ad" -- 365 Moderate or heavy sleet showers
weatherIcon 362 = "\xe3ad" -- 362 Light sleet showers
weatherIcon 350 = "\xe3ad" -- 350 Ice pellets
weatherIcon 320 = "\xe3ad" -- 320 Moderate or heavy sleet
weatherIcon 317 = "\xe3ad" -- 317 Light sleet
weatherIcon 185 = "\xe3ad" -- 185 Patchy freezing drizzle nearby
weatherIcon 182 = "\xe3ad" -- 182 Patchy sleet nearby
weatherIcon 377 = "\xe3ad" -- 377 Moderate or heavy showers of ice pellets
weatherIcon 311 = "\xe3ad" -- 311 Light freezing rain
weatherIcon 374 = "\xe3ad" -- 374 Light showers of ice pellets
weatherIcon 284 = "\xe3ad" -- 284 Heavy freezing drizzle  w
weatherIcon 281 = "\xe3ad" -- 281 Freezing drizzle
weatherIcon 314 = "\xe3ad" -- 314 Moderate or Heavy freezing rain

-- nf-weather-fog
weatherIcon 260 = "\xe313" -- 260 Freezing fog
weatherIcon 248 = "\xe313" -- 248 Fog
weatherIcon 143 = "\xe313" -- 143 Mist

-- nf-weather-cloud
weatherIcon 122 = "\xe33d" -- 122 Overcast
weatherIcon 119 = "\xe33d" -- 119 Cloudy
weatherIcon 116 = "\xe33d" -- 116 Partly Cloudy

-- nf-weather-day_sunny
weatherIcon 113 = "\xe30d" -- 113 Clear/Sunny
-- nf-weather-night_clear
weatherIcon 114 = "\xe32b" -- 113 Clear/Sunny (but at night!)
weatherIcon _ = ""

currentWeatherSection :: WeatherState -> L.Powerlemon
currentWeatherSection (WeatherState temp _ _ _ _ code desc _ _) = do
  let colors :: [(Float, C.Color)]
      colors = [(-5, C.blue), (8, C.dullGreen), (22, C.dullGreen), (35, C.red)]
      backColor = C.mixComplex colors $ fromIntegral temp
  L.openSection $ C.fromBackColor backColor
  L.write $ weatherIcon code
  L.write " "
  L.write desc
  L.write " "
  L.write $ show temp
  L.write "°C "
currentWeatherSection _ = error "something"

withLeading0 :: Int -> String
withLeading0 x
  | x < 10 = '0' : show x
  | otherwise = show x

formatDiff :: DiffTime -> String
formatDiff (timeToTimeOfDay -> TimeOfDay {todHour = hours, todMin = minutes}) = withLeading0 hours ++ ":" ++ withLeading0 minutes

formatRingDifference :: TimeOfDay -> TimeOfDay -> String
formatRingDifference a b
  | a < b = formatDiff $ timeOfDayToTime b - timeOfDayToTime a
  | otherwise = formatDiff $ secondsToDiffTime (60 * 60 * 24) + timeOfDayToTime b - timeOfDayToTime a

astronomySection :: WeatherState -> L.Powerlemon
astronomySection (WeatherState _ _ _ _ _ _ _ sunrise sunset) = do
  L.openSection C.neutralColorPair
  timeOfDay <- localTimeOfDay . zonedTimeToLocalTime <$> lift getZonedTime
  if timeOfDay < sunrise
    then do
      L.write "\xe34c in "
      L.write $ formatRingDifference timeOfDay sunrise
    else
      if timeOfDay < sunset
        then do
          L.write "\xe34d in "
          L.write $ formatRingDifference timeOfDay sunset
        else do
          L.write "\xe34c in "
          L.write $ formatRingDifference timeOfDay sunrise
astronomySection _ = error "something"

print :: MVar WeatherState -> Index -> L.Powerlemon
print shared _monitorIndex = withMVar shared $ \case
  NoWeatherState -> return ()
  weatherState@WeatherState {} -> do
    L.setStyle L.Common
    currentWeatherSection weatherState
    astronomySection weatherState
    L.write " "
    L.closeSection
