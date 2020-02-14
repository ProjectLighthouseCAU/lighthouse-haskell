module Lighthouse.API () where

import Control.Monad.Trans.Reader
import qualified Network.WebSockets as WS

-- | The central IO monad to be used by Lighthouse applications. Holds a connection.
type LighthouseIO a = ReaderT WS.Connection IO a
