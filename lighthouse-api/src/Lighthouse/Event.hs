module Lighthouse.Event (KeyEvent (..)) where

import Control.Monad.Trans.State

-- A key/controller event emitted via the web interface.
data KeyEvent = KeyEvent { eventSource :: Int,
                           eventKey :: Int,
                           eventPressed :: Bool,
                           eventIsController :: Bool }
