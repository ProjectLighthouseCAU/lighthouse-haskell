module Lighthouse.Connection (LighthouseIO (..), runLighthouseIO) where

import Control.Monad.Trans.Reader
import Lighthouse.Authentication
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS

data ConnectionState = ConnectionState { wsConnection :: WS.Connection, lhAuth :: Authentication }

-- | The central IO monad to be used by Lighthouse applications. Holds a connection.
type LighthouseIO a = ReaderT ConnectionState IO a

-- | Runs a lighthouse application using the given credentials.
runLighthouseIO :: LighthouseIO a -> Authentication -> IO a
runLighthouseIO lio auth = withSocketsDo $ WS.runClient "lighthouse.uni-kiel.de" 80 path
                                         $ \conn -> runReaderT lio $ ConnectionState { wsConnection = conn, lhAuth = auth }
    where path = "/user/" ++ show (username auth) ++ "/model"
