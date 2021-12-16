{-# LANGUAGE OverloadedStrings #-}
module Services.ConfigService (
  load,
  Configuration (..)
  ) where

import Data.Yaml
import Models.Configuration

-- Create configuration service with specific config-file
load :: IO Configuration
load = decodeFileThrow "app-settings.yaml"
