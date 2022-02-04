module Lighthouse.Options
    ( Options (..), defaultOptions
    , Authentication (..)
    ) where

import qualified Data.Text as T
import Lighthouse.Utils.Logging (LogHandler, noopLogHandler)

-- | Configuration options.
data Options s = Options
    { optAuthentication :: Authentication
    , optInitialState   :: s
    , optLogHandler     :: LogHandler
    , optCloseOnError   :: Bool
    }

-- | Authentication details for the Lighthouse.
data Authentication = Authentication
    { authUsername :: T.Text
    , authToken    :: T.Text
    }
    deriving (Show, Eq)

-- | Creates a default set of options.
defaultOptions :: Authentication -> s -> Options s
defaultOptions auth st = Options
    { optAuthentication = auth
    , optInitialState   = st
    , optLogHandler     = noopLogHandler
    , optCloseOnError   = True
    }
