{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module State.Weather
  ( WeatherState
      ( WeatherState,
        weatherTemp,
        weatherFeelsLike,
        weatherMinTemp,
        weatherMaxTemp,
        weatherHumidity,
        weatherWeatherCode,
        weatherWeatherDesc,
        weatherSunrise,
        weatherSunset,
        NoWeatherState
      ),
    init,
  )
where

import Control.Concurrent.MVar (MVar, newMVar)
import Data.Time (TimeOfDay)
import Prelude

data WeatherState
  = WeatherState
      { weatherTemp :: Int,
        weatherFeelsLike :: Int,
        weatherMinTemp :: Int,
        weatherMaxTemp :: Int,
        weatherHumidity :: Int,
        weatherWeatherCode :: Int,
        weatherWeatherDesc :: String,
        weatherSunrise :: TimeOfDay,
        weatherSunset :: TimeOfDay
      }
  | NoWeatherState

init :: IO (MVar WeatherState)
init = newMVar NoWeatherState
