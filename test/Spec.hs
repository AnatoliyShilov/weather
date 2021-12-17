{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import qualified Data.Text as T
import qualified Services.ConfigService as Config
import qualified Services.LoggerService.ConsoleLoggerService as ConsoleLogger
import qualified Services.LoggerService.LoggerService as Logger
import qualified Services.WeatherService.OpenWeatherService as OpenWeather
import qualified Services.WeatherService.WeatherService as Weather
import Network.HTTP.Client.Conduit (Response(responseBody, responseStatus))
import Network.HTTP.Types.Status (ok200)

main :: IO ()
main = hspec $ do
  describe "Open Weather" $ do
    it "Receive data from server" $ do
      config <- Config.load
      logger <- ConsoleLogger.new Logger.Debug
      weather <- OpenWeather.new config logger
      let request = Weather.requestFrom config
      response <- Weather.oneCall weather request
      Logger.logInfo logger $ show $ responseBody response
      responseStatus response `shouldBe` ok200

