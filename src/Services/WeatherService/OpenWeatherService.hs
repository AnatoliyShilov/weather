module Services.WeatherService.OpenWeatherService (new) where

import Data.Functor ( (<&>) )
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Simple as Http
import qualified Services.WeatherService.WeatherService as Weather
import qualified Models.Configuration as Config
import qualified Services.LoggerService.LoggerService as Logger

-- Create weather service handle
new :: Config.Configuration -> Logger.Handle -> IO Weather.Handle
new config logger =
  return $ Weather.Handle {
    Weather.oneCall = createOneCall config logger
    }

createOneCall :: Config.Configuration -> Logger.Handle -> Weather.Request -> IO (Http.Response Weather.Response)
createOneCall config logger params = do
  -- Create request from url and append query string
  request <- Http.parseRequest ("GET " <> T.unpack (Config.apiUrl config))
    <&> Http.setRequestQueryString [
      (TE.encodeUtf8 $ T.pack "appid", Just . TE.encodeUtf8 $ Weather.appid params),
      (TE.encodeUtf8 $ T.pack "exclude", Just . TE.encodeUtf8 $ Weather.exclude params),
      (TE.encodeUtf8 $ T.pack "lat", Just . TE.encodeUtf8 . T.pack . show $ Weather.lat params),
      (TE.encodeUtf8 $ T.pack "lon", Just . TE.encodeUtf8 . T.pack . show $ Weather.lon params),
      (TE.encodeUtf8 $ T.pack "units", Just . TE.encodeUtf8 $ Weather.units params),
      (TE.encodeUtf8 $ T.pack "lang", Just . TE.encodeUtf8 $ Weather.lang params)
      ]
  Logger.logDebug logger request
  Http.httpJSON request
