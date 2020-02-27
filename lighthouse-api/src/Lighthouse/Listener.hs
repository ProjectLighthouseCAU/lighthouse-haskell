module Lighthouse.Listener (Listener (..), ButtonEvent (..)) where

-- A listener for keyboard/controller events fired from the web interface.
data Listener e = Listener { keyboardEvent :: e -> IO (),
                             controllerEvent :: e -> IO () }

-- A button event emitted via the web interface.
data ButtonEvent = ButtonEvent { eventSource :: Int,
                                 eventButton :: Int,
                                 eventButtonDown :: Bool }
