module Lighthouse.Authentication
    ( Authentication (..)
    ) where

import qualified Data.Text as T

-- | Authentication details for the Lighthouse.
data Authentication = Authentication { username :: T.Text, token :: T.Text }
    deriving (Show, Eq)
