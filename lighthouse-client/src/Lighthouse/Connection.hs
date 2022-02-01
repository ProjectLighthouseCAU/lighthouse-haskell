{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Lighthouse.Connection
    ( -- * The LighthouseIO monad
      LighthouseIO (..), Listener (..)
    , emptyListener, runLighthouseIO
      -- * Communication with the lighthouse
    , sendRequest, sendDisplay, requestInputStream, sendClose
    , receiveEvent
    ) where

import Control.Monad ((<=<))
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.State
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (sequence_)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.MessagePack as MP
import qualified Data.Text as T
import Lighthouse.Authentication
import Lighthouse.Display
import Lighthouse.Protocol
import Lighthouse.Utils.General (whileM_)
import Lighthouse.Utils.Serializable
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import qualified Wuss as WSS

-- | Stores the WebSocket connection and the credentials.
data ConnectionState = ConnectionState
    { csConnection :: WS.Connection
    , csAuthentication :: Authentication
    , csRequestId :: Int
    , csClosed :: Bool
    }

-- | The central IO monad to be used by lighthouse applications. Holds a connection.
type LighthouseIO = StateT ConnectionState IO

-- | A listener for events from the server.
data Listener = Listener
    { onConnect :: LighthouseIO ()
    , onInput   :: InputEvent -> LighthouseIO ()
    , onError   :: T.Text     -> LighthouseIO ()
    , onWarning :: T.Text     -> LighthouseIO ()
    }

-- | Creates an empty listener.
emptyListener :: Listener
emptyListener = Listener
    { onConnect = return ()
    , onInput   = \_ -> return ()
    , onError   = \_ -> return ()
    , onWarning = \_ -> return ()
    }

-- | Passes an event to the given listener.
notifyListener :: ServerEvent -> Listener -> LighthouseIO ()
notifyListener ServerErrorEvent {..} l = do
    sequence_ (onError l <$> seError)
    mapM_ (onWarning l) seWarnings
notifyListener ServerInputEvent {..} l = mapM_ (onInput l) seEvents

-- | Runs a lighthouse application using the given credentials.
runLighthouseIO :: [Listener] -> Authentication -> IO ()
runLighthouseIO listeners auth = withSocketsDo $
    WSS.runSecureClient "lighthouse.uni-kiel.de" 443 "/websocket" $ \conn -> do
        let state = ConnectionState { csConnection = conn, csAuthentication = auth, csClosed = False, csRequestId = 0 }

        flip evalStateT state $ do
            mapM_ onConnect listeners

            -- Run event loop
            whileM_ (not <$> gets csClosed) $ do
                liftIO $ putStrLn $ "Receiving event..."
                e <- receiveEvent

                case e of
                    Left err -> liftIO $ putStrLn $ "Error decoding event: " ++ T.unpack err
                    Right e' -> do
                        liftIO $ putStrLn $ "Got event: " ++ show e'
                        mapM_ (notifyListener e') listeners

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
receive :: Deserializable a => LighthouseIO (Either T.Text a)
receive = deserialize <$> receiveBinaryData

-- | Sends a request to the lighthouse.
sendRequest :: ClientRequest -> LighthouseIO ()
sendRequest r = do
    auth <- gets csAuthentication
    reqId <- gets csRequestId
    modify $ \cs -> cs { csRequestId = reqId + 1 }
    send $ encodeRequest reqId auth r

-- | Receives an event from the lighthouse.
receiveEvent :: LighthouseIO (Either T.Text ServerEvent)
receiveEvent = (decodeEvent =<<) <$> receive

-- | Sends a display request with the given display.
sendDisplay :: Display -> LighthouseIO ()
sendDisplay = sendRequest . DisplayRequest

-- | Requests a stream of input events.
requestInputStream :: LighthouseIO ()
requestInputStream = sendRequest InputStreamRequest

-- | Sends a close message.
sendClose :: LighthouseIO ()
sendClose = do
    conn <- gets csConnection
    modify $ \cs -> cs { csClosed = True }
    liftIO $ WS.sendCloseCode conn status $ T.pack "end of data"
    where status = 1000 -- normal
