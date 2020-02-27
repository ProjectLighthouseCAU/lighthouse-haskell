module Lighthouse.Listener (Listener (..), ButtonEvent (..)) where

-- A listener for keyboard/controller events fired from the web interface.
data Listener e = { keyboardEvent :: e -> IO (),
                    controllerEvent :: e -> IO () }

-- A button event emitted via the web interface.
data ButtonEvent = { eventSource :: Int,
                     eventButton :: Int,
                     eventButtonDown :: Bool }
