{-# LANGUAGE OverloadedStrings #-}
module Services.LoggerService.ConsoleLoggerService (
  new,
  from,
  with
  ) where

import Control.Concurrent (
  newMVar,
  withMVar,
  MVar
  )
import Control.Monad ( when )
import Data.Maybe ( fromJust, isNothing )
import Control.Exception ( throw )
import Language.Haskell.HsColour.ANSI
import qualified Data.Text as T
import qualified Models.Configuration as Config
import qualified Services.LoggerService.LoggerService as LoggerService

data Handle = Handle {
  -- Minimum log message which will written
  priorityLevel :: LoggerService.Priority,
  -- Thread safe: messages will not overlapped
  mutex :: MVar ()
  }

-- Create new logger service from configuration
from :: Config.Configuration -> IO LoggerService.Handle
from config = new $ Config.logLevel config

-- Create new logger service
new :: LoggerService.Priority -> IO LoggerService.Handle
new logLevel = do
  mutex <- newMVar ()
  let handle = Handle {
    priorityLevel = logLevel,
    mutex = mutex
    }
  return LoggerService.Handle { LoggerService.log = createLog handle }

-- Perform IO action with new handle
with :: LoggerService.Priority -> (LoggerService.Handle -> IO a) -> IO a
with logLevel ioAction = new logLevel >>= ioAction

-- Implementation of log method
createLog :: Handle -> LoggerService.Priority -> T.Text -> IO ()
createLog handle priority message = do
  when (priority >= priorityLevel handle) $
    withMVar (mutex handle) $ \ () -> do
      let priorityName = highlight (appearence priority) $ show priority
      putStrLn $ "[" <> priorityName <> "] : " <> T.unpack message

appearence :: LoggerService.Priority -> [Highlight]
appearence LoggerService.Debug = [Foreground White]
appearence LoggerService.Info = [Foreground Green]
appearence LoggerService.Warning = [Foreground Yellow]
appearence LoggerService.Error = [Foreground Red]
