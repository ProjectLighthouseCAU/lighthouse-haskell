{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Lighthouse.Utils.Logging
    ( -- * Logging levels
      LogLevel (..)
    , errorLevel, warnLevel, infoLevel, debugLevel, traceLevel
      -- * Log messages
    , LogMessage (..)
      -- * Log handling
    , LogHandler, simpleLogHandler
    , MonadLogger (..)
      -- * Convenience functions
    , logError, logWarn, logInfo, logDebug, logTrace
    ) where

import Control.Monad (guard, void)
import Control.Monad.Trans (lift, MonadIO (..))
import Control.Monad.Trans.Except (ExceptT (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.State (StateT (..))
import Control.Monad.Trans.Writer (WriterT (..))
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

-- | The log level for trace messages.
traceLevel :: LogLevel
traceLevel = LogLevel (-2) "TRACE"

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
simpleLogHandler handlerLevel LogMessage {..} = void $ runMaybeT $ do
    guard (llValue lmLevel >= llValue handlerLevel)
    liftIO $ putStrLn $ T.unpack $ "[" <> llName lmLevel <> "] " <> lmOrigin <> ": " <> lmMessage

class Monad m => MonadLogger m where
    -- | Logs the given message within the monad.
    logMessage :: LogMessage -> m ()

instance MonadLogger m => MonadLogger (ExceptT e m) where
    logMessage = lift . logMessage

instance MonadLogger m => MonadLogger (ReaderT r m) where
    logMessage = lift . logMessage

instance MonadLogger m => MonadLogger (MaybeT m) where
    logMessage = lift . logMessage

instance MonadLogger m => MonadLogger (StateT s m) where
    logMessage = lift . logMessage

instance (Monoid w, MonadLogger m) => MonadLogger (WriterT w m) where
    logMessage = lift . logMessage

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

-- | Logs a message at the trace level.
logTrace :: MonadLogger m => T.Text -> T.Text -> m ()
logTrace o m = logMessage LogMessage { lmLevel = traceLevel, lmOrigin = o, lmMessage = m }
