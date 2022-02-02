module Lighthouse.Options
    ( Options (..)
    , Authentication (..)
    ) where

import qualified Data.Text as T
import Lighthouse.Utils.Logging (LogHandler)

-- | Configuration options.
data Options s = Options
    { optAuthentication :: Authentication
    , optLogHandler     :: LogHandler
    , optInitialState   :: s
    }

-- | Authentication details for the Lighthouse.
data Authentication = Authentication
    { authUsername :: T.Text
    , authToken    :: T.Text
    }
    deriving (Show, Eq)
