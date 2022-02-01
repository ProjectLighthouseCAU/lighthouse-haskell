{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Lighthouse.Protocol
    ( -- * Client -> server messages
      ClientRequest(..)
    , ClientMessage (..)
    , encodeRequest
     -- * Server -> client messages
    , ServerEvent (..), InputEvent (..), Input (..)
    , ServerMessage (..)
    , decodeEvent
    ) where

import Control.Applicative ((<|>))
import Control.Monad ((<=<), guard)
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (concat)
import Data.Maybe (isJust, mapMaybe)
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
    deriving (Show, Eq)

-- | Low-level client -> server message structure.
data ClientMessage = ClientMessage
    { cRequestId :: Int
    , cVerb :: T.Text
    , cPath :: [T.Text]
    , cAuthentication :: Authentication
    , cPayload :: MP.Object
    }
    deriving (Show, Eq)

-- | Encodes a ClientRequest to a ClientMessage.
encodeRequest :: Int -> Authentication -> ClientRequest -> ClientMessage
encodeRequest reqId auth (DisplayRequest disp) = ClientMessage
    { cRequestId = reqId
    , cVerb = "PUT"
    , cPath = ["user", username auth, "model"]
    , cAuthentication = auth
    , cPayload = mpSerialize disp
    }
encodeRequest reqId auth ControllerStreamRequest = ClientMessage
    { cRequestId = reqId
    , cVerb = "STREAM"
    , cPath = ["user", username auth, "model"]
    , cAuthentication = auth
    , cPayload = MP.ObjectNil
    }

instance MPSerializable ClientMessage where
    mpSerialize ClientMessage {..} = mpMap
        [ ("REID", mpInt cRequestId)
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
data ServerEvent = ServerErrorEvent { seWarnings :: [T.Text], seError :: Maybe T.Text }
                 | ServerInputEvent { seEvents :: [InputEvent] }
    deriving (Show, Eq)

-- | A key event emitted via the web interface.
data InputEvent = InputEvent
    { keSource :: Int
    , keInput :: Input
    , keIsDown :: Bool
    }
    deriving (Show, Eq)

-- | An input via the web interface.
data Input = KeyInput { iKey :: Int }
           | ControllerInput { iButton :: Int }
    deriving (Show, Eq)

-- | Low-level server -> client message structure.
data ServerMessage = ServerMessage
    { sRNum :: Int
    , sReqId :: Maybe Int
    , sWarnings :: [T.Text]
    , sResponse :: Maybe T.Text
    , sPayload :: Maybe MP.Object
    }
    deriving (Show, Eq)

-- | Decodes a ServerMessage to a ServerEvent.
decodeEvent :: ServerMessage -> Maybe ServerEvent
decodeEvent ServerMessage {..} = case sRNum of
    200 -> ServerInputEvent <$> (mpDeserialize =<< sPayload)
    _   -> do
        guard (not (null sWarnings) || isJust sResponse)
        Just $ ServerErrorEvent sWarnings sResponse

instance MPDeserializable ServerMessage where
    mpDeserialize (MP.ObjectMap vm) = do
        let m = V.toList vm
        rnum <- mpUnInt =<< lookup (mpStr "RNUM") m
        return ServerMessage
            { sRNum = rnum
            , sReqId = mpUnInt =<< lookup (mpStr "REOD") m
            , sResponse = mpUnStr =<< lookup (mpStr "RESPONSE") m
            , sWarnings = mapMaybe mpUnStr (concat (mpUnArray =<< lookup (mpStr "WARNINGS") m))
            , sPayload = lookup (mpStr "PAYL") m
            }
    mpDeserialize _ = Nothing

instance MPDeserializable InputEvent where
    mpDeserialize (MP.ObjectMap vo) = do
        let o = V.toList vo
        src <- mpUnInt =<< lookup (mpStr "src") o
        let key = mpUnInt =<< lookup (mpStr "key") o
            btn = mpUnInt =<< lookup (mpStr "btn") o
        input <- (KeyInput <$> key) <|> (ControllerInput <$> btn)
        dwn <- mpUnBool =<< lookup (mpStr "dwn") o
        return InputEvent
            { keSource = src
            , keInput = input
            , keIsDown = dwn
            }
    mpDeserialize _ = Nothing

instance Deserializable ServerMessage where
    deserialize = mpDeserialize <=< MP.unpack
