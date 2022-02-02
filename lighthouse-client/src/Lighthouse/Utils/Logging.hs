{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Lighthouse.Utils.Logging
    ( -- * Logging levels
      LogLevel (..)
    , errorLevel, warnLevel, infoLevel, debugLevel
      -- * Log messages
    , LogMessage (..)
      -- * Log handling
    , LogHandler, simpleLogHandler
    , MonadLogger (..)
      -- * Convenience functions
    , logError, logWarn, logInfo, logDebug
    ) where

import Control.Monad (guard)
import qualified Data.Text as T

-- | The level to log at.
data LogLevel = LogLevel { llValue :: Int, llName :: T.Text }

-- | The log level for errors.
errorLevel :: LogLevel
errorLevel = LogLevel 2 "ERROR"

-- | The log level for warnings.
warnLevel :: LogLevel
warnLevel = LogLevel 1 "WARN"

-- | The log level for informational messages.
infoLevel :: LogLevel
infoLevel = LogLevel 0 "INFO"

-- | The log level for debug messages.
debugLevel :: LogLevel
debugLevel = LogLevel (-1) "DEBUG"

-- | A logged message along with a level to log it at and an origin.
data LogMessage = LogMessage
    { lmLevel   :: LogLevel
    , lmOrigin  :: T.Text
    , lmMessage :: T.Text
    }

-- | A processor for log messages.
type LogHandler = LogMessage -> IO ()

-- | A simple stdout-based log handler.
simpleLogHandler :: LogLevel -> LogHandler
simpleLogHandler handlerLevel LogMessage {..} = do
    guard (llValue lmLevel >= llValue handlerLevel)
    putStrLn $ T.unpack $ "[" <> llName lmLevel <> "] " <> lmOrigin <> ": " <> lmMessage

class Monad m => MonadLogger m where
    -- | Logs the given message within the monad.
    logMessage :: LogMessage -> m ()

-- | Logs a message at the error level.
logError :: MonadLogger m => T.Text -> T.Text -> m ()
logError o m = logMessage LogMessage { lmLevel = errorLevel, lmOrigin = o, lmMessage = m }

-- | Logs a message at the warn level.
logWarn :: MonadLogger m => T.Text -> T.Text -> m ()
logWarn o m = logMessage LogMessage { lmLevel = warnLevel, lmOrigin = o, lmMessage = m }

-- | Logs a message at the info level.
logInfo :: MonadLogger m => T.Text -> T.Text -> m ()
logInfo o m = logMessage LogMessage { lmLevel = infoLevel, lmOrigin = o, lmMessage = m }

-- | Logs a message at the debug level.
logDebug :: MonadLogger m => T.Text -> T.Text -> m ()
logDebug o m = logMessage LogMessage { lmLevel = debugLevel, lmOrigin = o, lmMessage = m }
