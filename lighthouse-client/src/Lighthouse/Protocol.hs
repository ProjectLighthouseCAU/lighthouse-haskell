{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Lighthouse.Protocol
    ( -- * Client -> server messages
      ClientRequest(..)
    , ClientMessage (..)
    , encodeRequest
     -- * Server -> client messages
    , ServerEvent (..), KeyEvent (..)
    , ServerMessage (..)
    , decodeEvent
    ) where

import Control.Applicative ((<|>))
import Control.Monad ((<=<))
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (isJust)
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
                   | ControllerStreamRequest

-- | Low-level client -> server message structure.
data ClientMessage = ClientMessage
    { cReqId :: Int
    , cVerb :: T.Text
    , cPath :: [T.Text]
    , cAuthentication :: Authentication
    , cPayload :: MP.Object
    }

-- | Encodes a ClientRequest to a ClientMessage.
encodeRequest :: Authentication -> ClientRequest -> ClientMessage
encodeRequest auth (DisplayRequest disp) = ClientMessage
    { cReqId = 0
    , cVerb = "PUT"
    , cPath = ["user", username auth, "model"]
    , cAuthentication = auth
    , cPayload = mpSerialize disp
    }
encodeRequest auth ControllerStreamRequest = ClientMessage
    { cReqId = -1
    , cVerb = "STREAM"
    , cPath = ["user", username auth, "model"]
    , cAuthentication = auth
    , cPayload = MP.ObjectNil
    }

instance MPSerializable ClientMessage where
    mpSerialize ClientMessage {..} = mpMap
        [ ("REID", mpInt cReqId)
        , ("VERB", mpStr cVerb)
        , ("PATH", mpArray (mpStr <$> cPath))
        , ("AUTH", mpMap [("USER", mpStr username), ("TOKEN", mpStr token)])
        , ("META", mpMap [])
        , ("PAYL", cPayload)
        ]
        where Authentication {..} = cAuthentication

instance MPSerializable Display where
    mpSerialize = MP.ObjectBin . BL.toStrict . serialize

instance Serializable ClientMessage where
    serialize = MP.pack . mpSerialize

-- Server -> client messages

-- | High-level server -> client message structure.
data ServerEvent = ServerErrorEvent { seError :: T.Text }
                 | ServerKeysEvent { seEvents :: [KeyEvent] }

-- | A key event emitted via the web interface.
data KeyEvent = KeyEvent
    { keSource :: Int
    , keKeyCode :: Int
    , keIsController :: Bool
    , keIsDown :: Bool
    }
    deriving (Show, Eq)

-- | Low-level server -> client message structure.
data ServerMessage = ServerMessage
    { sRNum :: Int
    , sReqId :: Maybe Int
    , sResponse :: Maybe T.Text
    , sPayload :: Maybe MP.Object
    }

-- | Decodes a ServerMessage to a ServerEvent.
decodeEvent :: ServerMessage -> Maybe ServerEvent
decodeEvent ServerMessage {..} = case sRNum of
    200 -> ServerKeysEvent <$> (mpDeserialize =<< sPayload)
    _   -> ServerErrorEvent <$> sResponse

instance MPDeserializable ServerMessage where
    mpDeserialize (MP.ObjectMap vm) = do
        let m = V.toList vm
        rnum <- mpUnInt =<< lookup (mpStr "RNUM") m
        return ServerMessage
            { sRNum = rnum
            , sReqId = mpUnInt =<< lookup (mpStr "REOD") m
            , sResponse = mpUnStr =<< lookup (mpStr "RESPONSE") m
            , sPayload = lookup (mpStr "PAYL") m
            }
    mpDeserialize _ = Nothing

instance MPDeserializable KeyEvent where
    mpDeserialize (MP.ObjectMap vo) = do
        let o = V.toList vo
        src <- mpUnInt =<< lookup (mpStr "src") o
        let key = lookup (mpStr "key") o
            btn = lookup (mpStr "btn") o
        keyCode <- mpUnInt =<< (key <|> btn)
        dwn <- mpUnBool =<< lookup (mpStr "dwn") o
        return KeyEvent
            { keSource = src
            , keKeyCode = keyCode
            , keIsController = isJust btn
            , keIsDown = dwn
            }
    mpDeserialize _ = Nothing

instance Deserializable ServerMessage where
    deserialize = mpDeserialize <=< MP.unpack
