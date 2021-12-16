{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Services.LoggerService.LoggerService (
  Handle (..),
  Priority (..),
  WriteToLog,
  logDebug,
  logInfo,
  logWarning,
  logError
  ) where

import Prelude hiding (log)
import Control.Exception ( throw )
import Data.Yaml ( (.:), withObject, FromJSON(parseJSON), Parser )
import Exceptions.ParseException ( ParseException(ParseException) )
import qualified Network.HTTP.Simple as Http
import qualified Data.Text as T

-- Log message priorities
data Priority
  = Debug
  | Info
  | Warning
  | Error
  deriving (Eq, Ord, Show, Read)

-- Way to read priority from configuration
instance FromJSON Priority where
  parseJSON = withObject "Priority" $ \ v -> do
    logLevel <- v .: "log-level"  :: Parser T.Text
    return $ case logLevel of
      "Debug" -> Debug
      "Info" -> Info
      "Warning" -> Warning
      "Error" -> Error
      _ -> throw $ ParseException $ "Found " <> logLevel <> ", but it is undefined"

newtype Handle = Handle
  -- Write log message
  { log :: Priority -> T.Text -> IO () }

class (Show a) => WriteToLog a where
  writeToLog :: Handle -> Priority -> a -> IO ()
  writeToLog handle priority message = writeToLog handle priority (show message)

instance {-# OVERLAPPABLE #-} (Show a) => WriteToLog [a]

instance {-# OVERLAPING #-} WriteToLog String where
  writeToLog handle priority message = writeToLog handle priority (T.pack message)

instance WriteToLog T.Text where
  writeToLog handle priority message = log handle priority message

instance WriteToLog Char where
  writeToLog handle priority message = writeToLog handle priority (T.pack [message])

instance WriteToLog Http.Request

instance WriteToLog Float

instance WriteToLog Int

instance WriteToLog Integer

instance WriteToLog Double

instance (Show a, Show b) => WriteToLog (a, b)

instance (Show a, Show b, Show c) => WriteToLog (a, b, c)

-- Write Debug, Info, Warning, Error log message
logDebug, logInfo, logWarning, logError :: (WriteToLog a) => Handle -> a -> IO ()
logDebug = (`writeToLog` Debug)
logInfo = (`writeToLog` Info)
logWarning = (`writeToLog` Warning)
logError = (`writeToLog` Error)
