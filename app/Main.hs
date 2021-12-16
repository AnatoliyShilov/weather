module Main where

import Data.Time
import Control.Monad
import qualified Data.Text as T
import qualified Services.ConfigService as Config
import qualified Services.LoggerService.LoggerService as Logger
import qualified Services.LoggerService.ConsoleLoggerService as ConsoleLogger
import qualified Services.WeatherService.OpenWeatherService as OpenWeather
import qualified Services.WeatherService.WeatherService as Weather
import Network.HTTP.Client.Conduit (Response(responseBody))

main :: IO ()
main = do
  -- Load config from app-settings.yaml
  config <- Config.load
  -- Create logger service by cofig
  logger <- ConsoleLogger.from config
  -- Create weather service
  weather <- OpenWeather.new config logger
  -- Create request by config
  let request = Weather.requestFrom config
  -- Send OneCall request
  response <- Weather.oneCall weather request
  -- Get 5 day forecast
  let forecasts = take 5 . Weather.forecasts $ responseBody response
  -- Calculate initial sun day duration and difference of nightly temperatures
  let (firstSunDif, firstTempDif) = (getSunDif $ head forecasts, getTempDif $ head forecasts)
  -- Calculate sun day duration and difference of nightly temperatures
  let (sunDif, tempDif) = unzip $ map (\ forecast -> (
        getSunDif forecast,
        getTempDif forecast
        )) $ tail forecasts
  -- Day wich have a max sun day duration
  let maxSunDif = foldlBy (\ max item -> fst max < fst item)
        (firstSunDif, head forecasts) $
        zip sunDif $ tail forecasts
  -- Day wich have a min difference of nightly temperatures
  let minTempDif = foldlBy (\ min item -> fst min > fst item)
        (firstTempDif, head forecasts) $
        zip tempDif $ tail forecasts
  -- The local time zone
  timeZone <- getCurrentTimeZone
  Logger.logInfo logger "Максимальная продолжительность светового дня:"
  Logger.logInfo logger $ writeForecast timeZone $ snd maxSunDif
  Logger.logInfo logger "Минимальная разница \"ощущаемой\" и фактической ночных температур"
  Logger.logInfo logger $ writeForecast timeZone $ snd minTempDif
  Logger.logInfo logger "Все прогнозы"
  mapM_ (Logger.logInfo logger . writeForecast timeZone) forecasts
  where
    foldlBy cmp = foldl (\ acc item -> if cmp acc item then item else acc)
    -- Calculate sun day durations
    getSunDif forecast = Weather.sunset forecast `diffUTCTime` Weather.sunrise forecast
    -- Calculate difference of nightly temperatures
    getTempDif forecast = abs $ Weather.night forecast - Weather.feelsLikeNight forecast

-- Display forecast
writeForecast :: TimeZone -> Weather.Forecast -> String
writeForecast timeZone forecast  =
  "Прогноз погоды {" <>
  "\n\tДата = " <> writeUTC localDay (Weather.dt forecast) <>
  ",\n\tРассвет = " <> writeUTC localTimeOfDay (Weather.sunrise forecast) <>
  ",\n\tЗакат = " <> writeUTC localTimeOfDay (Weather.sunset forecast) <>
  ",\n\tТемпература ночью = " <> show (Weather.night forecast) <>
  "°C,\n\tОщущается как = " <> show (Weather.feelsLikeNight forecast) <> "°C }\n"
  where
    -- Convert UTC to local time and extract date or time
    writeUTC getter = show . getter . utcToLocalTime timeZone

