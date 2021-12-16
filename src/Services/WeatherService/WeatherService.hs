{-# LANGUAGE OverloadedStrings #-}
module Services.WeatherService.WeatherService where

import Data.Aeson ( (.:), withObject, FromJSON(parseJSON) )
import Data.Time ( UTCTime )
import Data.Time.Clock.POSIX
import qualified Network.HTTP.Simple as Http
import qualified Data.Text as T
import qualified Models.Configuration as Config

-- Request fields of OneCall method
data Request = Request {
  appid :: T.Text,
  lat :: Float,
  lon :: Float,
  exclude :: T.Text,
  units :: T.Text,
  lang :: T.Text
  } deriving (Show)

-- Create request by config
requestFrom :: Config.Configuration -> Request
requestFrom config = Request {
  appid = Config.appId config,
  lat = Config.lat config,
  lon = Config.lon config,
  exclude = Config.exclude config,
  units = Config.units config,
  lang = Config.lang config
  }

-- Response fields of OneCall method
newtype Response = Response {
  forecasts :: [Forecast]
  } deriving (Show)

data Forecast = Forecast {
  dt :: UTCTime,
  sunrise :: UTCTime,
  sunset :: UTCTime,
  night :: Float,
  feelsLikeNight :: Float
  } deriving (Show)

-- Parse JSON response into Response type
instance FromJSON Response where
  parseJSON = withObject "WeatherService.Response" $ \ v -> do Response
    <$> v .: "daily"

instance FromJSON Forecast where
  parseJSON = withObject "WeatherService.Forecast" $ \ v -> do
    dt <- v .: "dt"
    sunrise <- v .: "sunrise"
    sunset <- v .: "sunset"
    temp <- v .: "temp"
    night <- temp .: "night"
    feelsLike <- v .: "feels_like"
    feelsLikeNight <- feelsLike .: "night"
    return $ Forecast {
      dt = posixSecondsToUTCTime  dt,
      sunrise = posixSecondsToUTCTime sunrise,
      sunset = posixSecondsToUTCTime  sunset,
      night = night,
      feelsLikeNight = feelsLikeNight
      }

-- Weather service handle
newtype Handle = Handle {
  -- Send OneCall request to api
  oneCall :: Request -> IO (Http.Response Response)
  }
