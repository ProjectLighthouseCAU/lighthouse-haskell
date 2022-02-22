{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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

import Control.Monad ((<=<))
import qualified Data.ByteString.Lazy as BL
import Data.Either (fromRight)
import qualified Data.MessagePack as MP
import qualified Data.Text as T
import qualified Data.Vector as V
import Lighthouse.Display
import Lighthouse.Options (Authentication (..))
import Lighthouse.Utils.General (maybeToRight, rightToMaybe)
import Lighthouse.Utils.MessagePack
import Lighthouse.Utils.Serializable

-- Client -> server messages

-- | High-level client -> server message structure.
data ClientRequest = DisplayRequest { crDisplay :: Display }
                   | StreamRequest
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
encodeRequest reqId auth@Authentication {..} (DisplayRequest disp) = ClientMessage
    { cRequestId = reqId
    , cVerb = "PUT"
    , cPath = ["user", authUsername, "model"]
    , cAuthentication = auth
    , cPayload = mpSerialize disp
    }
encodeRequest reqId auth@Authentication {..} StreamRequest = ClientMessage
    { cRequestId = reqId
    , cVerb = "STREAM"
    , cPath = ["user", authUsername, "model"]
    , cAuthentication = auth
    , cPayload = MP.ObjectNil
    }

instance MPSerializable ClientMessage where
    mpSerialize ClientMessage {..} = mpMap
        [ ("REID", mpInt cRequestId)
        , ("VERB", mpStr cVerb)
        , ("PATH", mpArray (mpStr <$> cPath))
        , ("AUTH", mpMap [("USER", mpStr authUsername), ("TOKEN", mpStr authToken)])
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
data ServerEvent = ServerErrorEvent { seCode :: Int, seWarnings :: [T.Text], seError :: Maybe T.Text }
                 | ServerInputEvent { seEvent :: InputEvent }
                 | ServerUnknownEvent { sePayload :: MP.Object }
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
decodeEvent :: ServerMessage -> Either T.Text ServerEvent
decodeEvent ServerMessage {..} = case sRNum of
    200 -> do
        payload <- maybeToRight "Could not decode as input, no payload" sPayload
        Right $ fromRight (ServerUnknownEvent payload) $ ServerInputEvent <$> mpDeserialize payload
    _   -> Right $ ServerErrorEvent sRNum sWarnings sResponse

instance MPDeserializable ServerMessage where
    mpDeserialize (MP.ObjectMap vm) = do
        let m = V.toList vm
        rnum <- mpUnInt =<< mpLookup "RNUM" m
        return ServerMessage
            { sRNum = rnum
            , sReqId = rightToMaybe (mpUnInt =<< mpLookup "REOD" m)
            , sResponse = rightToMaybe (mpUnStr =<< mpLookup "RESPONSE" m)
            , sWarnings = fromRight [] (traverse mpUnStr (concat (mpUnArray =<< mpLookup "WARNINGS" m)))
            , sPayload = rightToMaybe (mpLookup "PAYL" m)
            }
    mpDeserialize o = Left $ "Could not deserialize as server message (not a map): " <> T.pack (show o)

instance MPDeserializable InputEvent where
    mpDeserialize (MP.ObjectMap vo) = do
        let o = V.toList vo
        src <- mpUnInt =<< mpLookup "src" o
        let key = mpUnInt =<< mpLookup "key" o
            btn = mpUnInt =<< mpLookup "btn" o
        input <- (KeyInput <$> key) <> (ControllerInput <$> btn)
        dwn <- mpUnBool =<< mpLookup "dwn" o
        return InputEvent
            { keSource = src
            , keInput = input
            , keIsDown = dwn
            }
    mpDeserialize o = Left $ "Could not deserialize as input event (not a map): " <> T.pack (show o)

instance Deserializable ServerMessage where
    deserialize = mpDeserialize <=< (maybeToRight errMsg . MP.unpack)
        where errMsg = "Could not deserialize server message as MessagePack object" 
