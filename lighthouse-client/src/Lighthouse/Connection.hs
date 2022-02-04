{-# LANGUAGE OverloadedStrings, RecordWildCards, GeneralizedNewtypeDeriving #-}
module Lighthouse.Connection
    ( -- * The LighthouseIO monad
      LighthouseIO (..), Listener (..)
    , runLighthouseApp, runLighthouseIO, getUserState, putUserState, modifyUserState
      -- * Communication with the lighthouse
    , sendRequest, sendDisplay, requestStream, sendClose
    , receiveEvent
    ) where

import Control.Monad ((<=<), when)
import Control.Monad.State.Class (MonadState (..), gets, modify)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.State (StateT, evalStateT)
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (traverse_)
import Data.Maybe (fromJust, fromMaybe, isJust)
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
data ConnectionState s = ConnectionState
    { csConnection :: WS.Connection
    , csOptions    :: Options s
    , csRequestId  :: Int
    , csClosed     :: Bool
    , csUserState  :: s
    }

-- Implementation note: We use the trick from https://stackoverflow.com/a/32572657 to
-- define our monad transformer stack using a newtype and derive most instances with
-- GeneralizedNewtypeDeriving.

-- | The central IO-ish monad to be used by lighthouse applications. Holds a connection.
newtype LighthouseIO s a = LighthouseIO (StateT (ConnectionState s) IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadState (ConnectionState s))

instance MonadLogger (LighthouseIO s) where
    logMessage m = do
        handleMessage <- gets (optLogHandler . csOptions)
        liftIO $ handleMessage m

-- | A listener for events from the server.
data Listener s = Listener
    { onConnect :: LighthouseIO s ()
    , onInput   :: InputEvent -> LighthouseIO s ()
    }

instance Semigroup (Listener s) where
    l1 <> l2 = Listener
        { onConnect = onConnect l1 >> onConnect l2
        , onInput   = \i -> onInput l1 i >> onInput l2 i
        }

instance Monoid (Listener s) where
    mempty = Listener
        { onConnect = return ()
        , onInput   = \_ -> return ()
        }

-- | Passes an event to the given listener.
notifyListener :: ServerEvent -> Listener s -> LighthouseIO s ()
notifyListener e l = case e of
    ServerInputEvent {..} -> onInput l seEvent
    _                     -> return ()

-- | Runs a lighthouse application using the given credentials.
runLighthouseApp :: Listener s -> Options s -> IO ()
runLighthouseApp listener = runLighthouseIO $ do
    onConnect listener

    -- Run event loop
    whileM_ (not <$> gets csClosed) $ do
        logDebug "runLighthouseApp" "Receiving event..."
        eventOrErr <- receiveEvent

        case eventOrErr of
            Left err -> logWarn "runLighthouseApp" $ "Could not parse event: " <> err

            Right ServerErrorEvent {..} -> do
                traverse_ onWarning seWarnings
                traverse_ onError seError

                closeOnError <- gets (optCloseOnError . csOptions)
                when (isJust seError && closeOnError)
                    sendClose

            Right ServerUnknownEvent {..} -> logDebug "runLighthouseApp" $ "Got unknown event: " <> T.pack (show sePayload)

            Right event -> do
                logDebug "runLighthouseApp" $ "Got event: " <> T.pack (show event)
                notifyListener event listener
    
    where onWarning w = logWarn "runLighthouseApp" $ "Server warning: " <> w
          onError   e = logError "runLighthouseApp" $ "Server error: " <> e

-- | Runs a single LighthouseIO using the given credentials.
runLighthouseIO :: LighthouseIO s a -> Options s -> IO a
runLighthouseIO (LighthouseIO lio) opts = withSocketsDo $
    WSS.runSecureClient "lighthouse.uni-kiel.de" 443 "/websocket" $ \conn -> do
        let state = ConnectionState { csConnection = conn
                                    , csOptions = opts
                                    , csClosed = False
                                    , csRequestId = 0
                                    , csUserState = optInitialState opts
                                    }
        evalStateT lio state

-- | Fetches the user state from the LighthouseIO monad.
getUserState :: LighthouseIO s s
getUserState = gets csUserState

-- | Updates the user state from the LighthouseIO monad.
putUserState :: s -> LighthouseIO s ()
putUserState = modifyUserState . const

-- | Modifies the user state from the LighthouseIO monad.
modifyUserState :: (s -> s) -> LighthouseIO s ()
modifyUserState f = modify $ \cs -> cs { csUserState = f (csUserState cs) }

-- | Sends raw, binary data directly to the lighthouse.
sendBinaryData :: BL.ByteString -> LighthouseIO s ()
sendBinaryData d = do
    conn <- gets csConnection
    liftIO $ WS.sendBinaryData conn d

-- | Receives raw, binary data directly from the lighthouse.
receiveBinaryData :: LighthouseIO s BL.ByteString
receiveBinaryData = do
    conn <- gets csConnection
    liftIO $ WS.receiveData conn

-- | Send a serializable value to the lighthouse.
send :: Serializable a => a -> LighthouseIO s ()
send = sendBinaryData . serialize

-- | Receives a deserializable value from the lighthouse.
receive :: Deserializable a => LighthouseIO s (Either T.Text a)
receive = deserialize <$> receiveBinaryData

-- | Sends a request to the lighthouse.
sendRequest :: ClientRequest -> LighthouseIO s ()
sendRequest r = do
    auth <- gets (optAuthentication . csOptions)
    reqId <- gets csRequestId
    modify $ \cs -> cs { csRequestId = reqId + 1 }
    send $ encodeRequest reqId auth r

-- | Receives an event from the lighthouse.
receiveEvent :: LighthouseIO s (Either T.Text ServerEvent)
receiveEvent = runExceptT $ do
    raw <- ExceptT receive
    logTrace "receiveEvent" $ "Got " <> T.pack (show raw)
    ExceptT $ return $ decodeEvent raw

-- | Sends a display request with the given display.
sendDisplay :: Display -> LighthouseIO s ()
sendDisplay = sendRequest . DisplayRequest

-- | Requests a stream of the model, including input events and the display (though the latter is undocumented).
requestStream :: LighthouseIO s ()
requestStream = sendRequest StreamRequest

-- | Sends a close message.
sendClose :: LighthouseIO s ()
sendClose = do
    conn <- gets csConnection
    modify $ \cs -> cs { csClosed = True }
    liftIO $ WS.sendCloseCode conn status $ T.pack "end of data"
    where status = 1000 -- normal
