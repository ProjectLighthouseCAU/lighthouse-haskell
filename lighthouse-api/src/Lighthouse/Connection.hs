module Lighthouse.Connection (LighthouseIO (..), runLighthouseIO, sendDisplay, sendClose) where

import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Lighthouse.Authentication
import Lighthouse.Display
import Lighthouse.Protocol
import Lighthouse.Utils.Serializable
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import qualified Wuss as WSS

data ConnectionState = ConnectionState { wsConnection :: WS.Connection, lhAuth :: Authentication }

-- | The central IO monad to be used by Lighthouse applications. Holds a connection.
type LighthouseIO a = ReaderT ConnectionState IO a

-- | Runs a lighthouse application using the given credentials.
runLighthouseIO :: LighthouseIO a -> Authentication -> IO a
runLighthouseIO lio auth = withSocketsDo $ WSS.runSecureClient "lighthouse.uni-kiel.de" 443 path
                                         $ \conn -> runReaderT lio $ ConnectionState { wsConnection = conn, lhAuth = auth }
    where path = "/user/" ++ T.unpack (username auth) ++ "/model"

-- | Sends raw, binary data directly to the lighthouse.
sendBinaryData :: BL.ByteString -> LighthouseIO ()
sendBinaryData d = do
    conn <- wsConnection <$> ask
    liftIO $ WS.sendBinaryData conn d

-- | Sends a display request with the given display.
sendDisplay :: Display -> LighthouseIO ()
sendDisplay d = do
    auth <- lhAuth <$> ask
    sendBinaryData $ serialize $ displayRequest auth d

-- | Sends a close message.
sendClose :: LighthouseIO ()
sendClose = do
    conn <- wsConnection <$> ask
    liftIO $ WS.sendCloseCode conn status $ T.pack "end of data"
    where status = 1000 -- normal
