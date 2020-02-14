module Lighthouse.Authentication (Authentication (..)) where

-- | Authentication details for the Lighthouse.
data Authentication = Authentication { username :: String, token :: String }
