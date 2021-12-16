{-# LANGUAGE OverloadedStrings #-}
module Models.Configuration where

import Data.Yaml ( FromJSON(parseJSON), withObject, (.:) )
import qualified Data.Text as T
import qualified Services.LoggerService.LoggerService as LoggerService

data Configuration = Configuration {
  apiUrl :: T.Text,
  appId :: T.Text,
  lat :: Float,
  lon :: Float,
  exclude :: T.Text,
  units :: T.Text,
  lang :: T.Text,
  logLevel :: LoggerService.Priority
  } deriving (Show)

instance FromJSON Configuration where
  parseJSON = withObject "Configuration" $ \ v -> do Configuration
    <$> v .: "api-url"
    <*> v .: "app-id"
    <*> v .: "lat"
    <*> v .: "lon"
    <*> v .: "exclude"
    <*> v .: "units"
    <*> v .: "lang"
    <*> v .: "logger"
