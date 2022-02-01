module Lighthouse.Event
    ( Listener (..)
    , KeyEvent (..)
    ) where

-- A listener for keyboard/controller events fired from the web interface.
data Listener e = Listener
    { keyboardEvent :: e -> IO ()
    , controllerEvent :: e -> IO ()
    }

-- A key event emitted via the web interface.
data KeyEvent = KeyEvent
    { eventSource :: Int
    , eventKey :: Int
    , eventPressed :: Bool
    }
    deriving (Show, Eq)
