{-# LANGUAGE OverloadedStrings, RecordWildCards, GeneralizedNewtypeDeriving #-}
module Lighthouse.Connection
    ( -- * The LighthouseIO monad
      LighthouseIO (..), Listener (..)
    , runLighthouseApp, runLighthouseIO
      -- * Communication with the lighthouse
    , sendRequest, sendDisplay, requestStream, sendClose
    , receiveEvent
    ) where

import Control.Monad ((<=<))
import Control.Monad.State.Class (MonadState (..), gets, modify)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.State (StateT, evalStateT)
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (sequence_)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.MessagePack as MP
import qualified Data.Text as T
import Lighthouse.Display
import Lighthouse.Options
import Lighthouse.Protocol
import Lighthouse.Utils.General (whileM_)
import Lighthouse.Utils.Logging
import Lighthouse.Utils.Serializable
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import qualified Wuss as WSS

-- | Stores the WebSocket connection and the credentials.
data ConnectionState = ConnectionState
    { csConnection :: WS.Connection
    , csOptions    :: Options
    , csRequestId  :: Int
    , csClosed     :: Bool
    }

-- Implementation note: We use the trick from https://stackoverflow.com/a/32572657 to
-- define our monad transformer stack using a newtype and derive most instances with
-- GeneralizedNewtypeDeriving.

-- | The central IO-ish monad to be used by lighthouse applications. Holds a connection.
newtype LighthouseIO a = LighthouseIO (StateT ConnectionState IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadState ConnectionState)

instance MonadLogger LighthouseIO where
    logMessage m = do
        handleMessage <- gets (optLogHandler . csOptions)
        liftIO $ handleMessage m

-- | A listener for events from the server.
data Listener = Listener
    { onConnect :: LighthouseIO ()
    , onInput   :: InputEvent -> LighthouseIO ()
    , onError   :: T.Text     -> LighthouseIO ()
    , onWarning :: T.Text     -> LighthouseIO ()
    }

instance Semigroup Listener where
    l1 <> l2 = Listener
        { onConnect = onConnect l1 >> onConnect l2
        , onInput   = \i -> onInput l1 i >> onInput l2 i
        , onError   = \e -> onError l1 e >> onError l2 e
        , onWarning = \w -> onWarning l1 w >> onWarning l2 w
        }

instance Monoid Listener where
    mempty = Listener
        { onConnect = return ()
        , onInput   = \_ -> return ()
        , onError   = \_ -> return ()
        , onWarning = \_ -> return ()
        }

-- | Passes an event to the given listener.
notifyListener :: ServerEvent -> Listener -> LighthouseIO ()
notifyListener e l = case e of
    ServerErrorEvent {..} -> do
        sequence_ (onError l <$> seError)
        mapM_ (onWarning l) seWarnings
    ServerInputEvent {..} -> onInput l seEvent
    _                     -> return ()

-- | Runs a lighthouse application using the given credentials.
runLighthouseApp :: Listener -> Options -> IO ()
runLighthouseApp listener = runLighthouseIO $ do
    onConnect listener

    -- Run event loop
    whileM_ (not <$> gets csClosed) $ do
        logDebug "runLighthouseApp" "Receiving event..."
        e <- receiveEvent

        case e of
            Left err -> logWarn "runLighthouseApp" $ "Got unrecognized event: " <> err
            Right e' -> do
                logDebug "runLighthouseApp" $ "Got event: " <> T.pack (show e')
                notifyListener e' listener

-- | Runs a single LighthouseIO using the given credentials.
runLighthouseIO :: LighthouseIO a -> Options -> IO a
runLighthouseIO (LighthouseIO lio) opts = withSocketsDo $
    WSS.runSecureClient "lighthouse.uni-kiel.de" 443 "/websocket" $ \conn -> do
        let state = ConnectionState { csConnection = conn, csOptions = opts, csClosed = False, csRequestId = 0 }
        evalStateT lio state

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
    auth <- gets (optAuthentication . csOptions)
    reqId <- gets csRequestId
    modify $ \cs -> cs { csRequestId = reqId + 1 }
    send $ encodeRequest reqId auth r

-- | Receives an event from the lighthouse.
receiveEvent :: LighthouseIO (Either T.Text ServerEvent)
receiveEvent = runExceptT $ do
    raw <- ExceptT receive
    logDebug "receiveEvent" $ "Got " <> T.pack (show raw)
    ExceptT $ return $ decodeEvent raw

-- | Sends a display request with the given display.
sendDisplay :: Display -> LighthouseIO ()
sendDisplay = sendRequest . DisplayRequest

-- | Requests a stream of the model, including input events and the display (though the latter is undocumented).
requestStream :: LighthouseIO ()
requestStream = sendRequest StreamRequest

-- | Sends a close message.
sendClose :: LighthouseIO ()
sendClose = do
    conn <- gets csConnection
    modify $ \cs -> cs { csClosed = True }
    liftIO $ WS.sendCloseCode conn status $ T.pack "end of data"
    where status = 1000 -- normal
