{-# LANGUAGE RecordWildCards #-}
module Lighthouse.Connection
    ( -- * The LighthouseIO monad
      LighthouseIO (..)
    , runLighthouseIO
      -- * Communication with the lighthouse
    , sendDisplay, sendClose
    , receiveKeyEvents
    ) where

import Control.Monad ((<=<))
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.State
import qualified Data.ByteString.Lazy as BL
import qualified Data.MessagePack as MP
import qualified Data.Text as T
import Lighthouse.Authentication
import Lighthouse.Display
import Lighthouse.Protocol
import Lighthouse.Utils.Serializable
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import qualified Wuss as WSS
import Data.Maybe (fromJust)

-- TODO: Maintain a list of listeners that get notified of key events
--       in this state and use forkIO to receive events from the connection
--       in an infinite loop (using forever).
-- | Stores the WebSocket connection and the credentials.
data ConnectionState = ConnectionState { wsConnection :: WS.Connection, lhAuth :: Authentication }

-- | The central IO monad to be used by lighthouse applications. Holds a connection.
type LighthouseIO a = StateT ConnectionState IO a

-- | A listener for keyboard/controller events fired from the web interface.
data Listener e = Listener
    { keyboardEvent :: e -> IO ()
    , controllerEvent :: e -> IO ()
    }

-- | Runs a lighthouse application using the given credentials.
runLighthouseIO :: LighthouseIO a -> Authentication -> IO a
runLighthouseIO lio auth = withSocketsDo $ WSS.runSecureClient "lighthouse.uni-kiel.de" 443 path
                                         $ \conn -> fst <$> (runStateT lio $ ConnectionState { wsConnection = conn, lhAuth = auth })
    where path = "/websocket"

-- | Sends raw, binary data directly to the lighthouse.
sendBinaryData :: BL.ByteString -> LighthouseIO ()
sendBinaryData d = do
    conn <- gets wsConnection
    liftIO $ WS.sendBinaryData conn d

-- | Receives raw, binary data directly from the lighthouse.
receiveBinaryData :: LighthouseIO BL.ByteString
receiveBinaryData = do
    conn <- gets wsConnection
    liftIO $ WS.receiveData conn

-- | Send a serializable value to the lighthouse.
send :: Serializable a => a -> LighthouseIO ()
send = sendBinaryData . serialize

-- | Receives a deserializable value from the lighthouse.
receive :: Deserializable a => LighthouseIO (Maybe a)
receive = deserialize <$> receiveBinaryData

-- | Sends a display request with the given display.
sendDisplay :: Display -> LighthouseIO ()
sendDisplay d = do
    auth <- lhAuth <$> get
    send $ encodeRequest auth $ DisplayRequest d

-- | Receives a batch of key event from the Lighthouse.
receiveKeyEvents :: LighthouseIO [KeyEvent]
receiveKeyEvents = do
    dat <- receive
    case decodeEvent =<< dat of
        Just ServerKeysEvent {..} -> return seEvents
        Just ServerErrorEvent {..} -> error $ "Got error from server: " ++ T.unpack seError
        _ -> error "Got unrecognized response to key events from server"

-- | Sends a close message.
sendClose :: LighthouseIO ()
sendClose = do
    conn <- wsConnection <$> get
    liftIO $ WS.sendCloseCode conn status $ T.pack "end of data"
    where status = 1000 -- normal
