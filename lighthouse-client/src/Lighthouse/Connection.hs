{-# LANGUAGE RecordWildCards #-}
module Lighthouse.Connection
    ( -- * The LighthouseIO monad
      LighthouseIO (..), Listener (..)
    , emptyListener, runLighthouseIO
      -- * Communication with the lighthouse
    , sendRequest, sendDisplay, sendClose
    , receiveEvent, receiveInputEvents
    ) where

import Control.Concurrent (forkIO)
import Control.Monad ((<=<), forever)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
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

-- | Stores the WebSocket connection and the credentials.
data ConnectionState = ConnectionState
    { csConnection :: WS.Connection
    , csAuthentication :: Authentication
    }

-- | The central IO monad to be used by lighthouse applications. Holds a connection.
type LighthouseIO = StateT ConnectionState IO

-- | A listener for events from the server.
data Listener = Listener
    { onConnect :: LighthouseIO ()
    , onInput :: InputEvent -> LighthouseIO ()
    , onError :: T.Text     -> LighthouseIO ()
    }

-- | Creates an empty listener.
emptyListener :: Listener
emptyListener = Listener
    { onConnect = return ()
    , onInput = \_ -> return ()
    , onError = \_ -> return ()
    }

-- | Passes an event to the given listener.
notifyListener :: ServerEvent -> Listener -> LighthouseIO ()
notifyListener ServerErrorEvent {..} l = onError l seError
notifyListener ServerInputEvent {..} l = mapM_ (onInput l) seEvents

-- | Runs a lighthouse application using the given credentials.
runLighthouseIO :: [Listener] -> Authentication -> IO a
runLighthouseIO listeners auth = withSocketsDo $
    WSS.runSecureClient "lighthouse.uni-kiel.de" 443 "/websocket" $ \conn -> do
        let state = ConnectionState { csConnection = conn, csAuthentication = auth }

        flip evalStateT state $ do
            mapM_ onConnect listeners

            -- Run event loop
            forever $ runMaybeT $ do
                ev <- MaybeT receiveEvent
                liftIO $ putStrLn $ "Got event: " ++ show ev
                mapM (lift . notifyListener ev) listeners

-- | Sends raw, binary data directly to the lighthouse.
sendBinaryData :: BL.ByteString -> LighthouseIO ()
sendBinaryData d = do
    conn <- gets csConnection
    liftIO $ WS.sendBinaryData conn d

-- | Receives raw, binary data directly from the lighthouse.
receiveBinaryData :: LighthouseIO BL.ByteString
receiveBinaryData = do
    conn <- gets csConnection
    liftIO $ WS.receiveData conn

-- | Send a serializable value to the lighthouse.
send :: Serializable a => a -> LighthouseIO ()
send = sendBinaryData . serialize

-- | Receives a deserializable value from the lighthouse.
receive :: Deserializable a => LighthouseIO (Maybe a)
receive = deserialize <$> receiveBinaryData

-- | Sends a request to the lighthouse.
sendRequest :: ClientRequest -> LighthouseIO ()
sendRequest r = do
    auth <- gets csAuthentication
    send $ encodeRequest auth r

-- | Receives an event from the lighthouse.
receiveEvent :: LighthouseIO (Maybe ServerEvent)
receiveEvent = (decodeEvent =<<) <$> receive

-- | Sends a display request with the given display.
sendDisplay :: Display -> LighthouseIO ()
sendDisplay = sendRequest . DisplayRequest

-- | Receives a batch of input events from the Lighthouse.
receiveInputEvents :: LighthouseIO [InputEvent]
receiveInputEvents = do
    e <- receiveEvent
    case e of
        Just ServerInputEvent {..} -> return seEvents
        Just ServerErrorEvent {..} -> error $ "Got error from server: " ++ T.unpack seError
        _ -> error "Got unrecognized response to key events from server"

-- | Sends a close message.
sendClose :: LighthouseIO ()
sendClose = do
    conn <- gets csConnection
    liftIO $ WS.sendCloseCode conn status $ T.pack "end of data"
    where status = 1000 -- normal
