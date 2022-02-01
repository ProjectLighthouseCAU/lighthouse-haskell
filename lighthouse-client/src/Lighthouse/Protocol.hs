{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Lighthouse.Protocol
    ( -- * Client -> server messages
      ClientRequest(..)
    , ClientMessage (..)
     -- * Server -> client messages
    , ServerEvent (..)
    , ServerMessage (..)
    ) where

import Control.Applicative ((<|>))
import Control.Monad ((<=<))
import qualified Data.ByteString.Lazy as BL
import qualified Data.MessagePack as MP
import qualified Data.Text as T
import qualified Data.Vector as V
import Lighthouse.Authentication
import Lighthouse.Display
import Lighthouse.Utils.General ((<.$>))
import Lighthouse.Utils.MessagePack
import Lighthouse.Utils.Serializable

-- Client -> server messages

-- | High-level client -> server message structure.
data ClientRequest = DisplayRequest { crDisplay :: Display }
                   | ControllerStream

-- | Low-level client -> server message structure.
data ClientMessage = ClientMessage
    { cReqId :: Int
    , cVerb :: T.Text
    , cPath :: [T.Text]
    , cAuthentication :: Authentication
    , cPayload :: MP.Object
    }

instance MPSerializable a => MPSerializable (ClientMessage a) where
    mpSerialize ClientRequest {..} = MP.ObjectMap $ V.fromList
        [ (MP.ObjectStr "REID", MP.ObjectInt cReqId)
        , (MP.ObjectStr "VERB", MP.ObjectStr cVerb)
        , (MP.ObjectStr "PATH", MP.ObjectArray $ V.fromList (MP.ObjectStr <$> cPath))
        , (MP.ObjectStr "AUTH", MP.ObjectMap $ V.fromList [(MP.ObjectStr "USER", MP.ObjectStr username), (MP.ObjectStr "TOKEN", MP.ObjectStr token)])
        , (MP.ObjectStr "META", MP.ObjectMap V.empty)
        , (MP.ObjectStr "PAYL", mpSerialize cPayload)
        ]
        where Authentication {..} = cAuthentication

instance MPSerializable a => Serializable (ClientMessage a) where
    serialize = MP.pack . mpSerialize

instance MPSerializable Display where
    mpSerialize = MP.ObjectBin . BL.toStrict . serialize

-- | Creates a display request.
displayRequest :: Authentication -> Display -> ClientMessage Display
displayRequest auth disp = ClientRequest
    { cReqId = 0
    , cVerb = "PUT"
    , cPath = ["user", username auth, "model"]
    , cAuthentication = auth
    , cPayload = disp
    }

-- | Creates a request for controller stream input.
controllerStreamRequest :: Authentication -> ClientMessage MP.Object
controllerStreamRequest auth = ClientRequest
    { cReqId = -1
    , cVerb = "STREAM"
    , cPath = ["user", username auth, "model"]
    , cAuthentication = auth
    , cPayload = MP.ObjectNil
    }

-- Server -> client messages

-- | High-level server -> client message structure.
data ServerEvent = ServerErrorEvent { srError :: T.Text }
                 | ServerKeysEvent { srEvents :: [KeyEvent] }

-- | A key event emitted via the web interface.
data KeyEvent = KeyEvent
    { keSource :: Int
    , keKeyCode :: Int
    , keIsController :: Bool
    , kePressed :: Bool
    }
    deriving (Show, Eq)

-- | Low-level server -> client message structure.
data ServerMessage = ServerMessage
    { sReqId :: Int
    , sRNum :: Int
    , sResponse :: T.Text
    , sPayload :: MP.Object
    }

instance MPDeserializable a => MPDeserializable (ServerMessage a) where
    mpDeserialize (MP.ObjectMap vm) = do
        let m = V.toList vm
        rnum <- MP.fromObject =<< lookup (MP.ObjectStr "RNUM") m
        case rnum :: Int of
            200 -> do
                reqId <- MP.fromObject =<< lookup (MP.ObjectStr "REID") m
                payload <- mpDeserialize =<< lookup (MP.ObjectStr "PAYL") m
                return $ ServerRequest { sReqId = reqId, sPayload = payload }
            _   -> do
                response <- MP.fromObject =<< lookup (MP.ObjectStr "RESPONSE") m
                return $ ServerError { sError = response }
    mpDeserialize _ = Nothing

instance MPDeserializable KeyEvent where
    mpDeserialize (MP.ObjectMap vo) = do
        let o = V.toList vo
        src <- MP.fromObject =<< lookup (MP.ObjectStr "src") o
        let key = lookup (MP.ObjectStr "key") o
            btn = lookup (MP.ObjectStr "btn") o
        code <- MP.fromObject =<< (key <|> btn)
        dwn <- MP.fromObject =<< lookup (MP.ObjectStr "dwn") o
        return $ KeyEvent
            { keSource = src
            , keKeyCode = key
            , keIsController = isJust btn
            , kePressed = dwn
            }
    mpDeserialize _ = Nothing

instance MPDeserializable a => Deserializable (ServerMessage a) where
    deserialize = mpDeserialize <=< MP.unpack
