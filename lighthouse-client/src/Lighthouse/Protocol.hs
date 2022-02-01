{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Lighthouse.Protocol
    ( ClientMessage (..), ServerMessage (..)
    , displayRequest, controllerStreamRequest
    ) where

import Control.Applicative ((<|>))
import Control.Monad ((<=<))
import qualified Data.ByteString.Lazy as BL
import qualified Data.MessagePack as MP
import qualified Data.Text as T
import qualified Data.Vector as V
import Lighthouse.Authentication
import Lighthouse.Display
import Lighthouse.Event
import Lighthouse.Utils.Serializable

-- * Client -> Server messages

data ClientMessage a = ClientRequest { cReqId :: Int, cVerb :: T.Text, cAuthentication :: Authentication, cPayload :: a }

class MPSerializable a where
    -- | Converts to a MessagePack representation.
    mpSerialize :: a -> MP.Object

instance MPSerializable a => MPSerializable (ClientMessage a) where
    mpSerialize ClientRequest {..} = MP.ObjectMap $ V.fromList
        [ (MP.ObjectStr "REID", MP.ObjectInt cReqId)
        , (MP.ObjectStr "VERB", MP.ObjectStr cVerb)
        , (MP.ObjectStr "PATH", MP.ObjectArray $ V.fromList (MP.ObjectStr <$> ["user", username, "model"]))
        , (MP.ObjectStr "AUTH", MP.ObjectMap $ V.fromList [(MP.ObjectStr "USER", MP.ObjectStr username), (MP.ObjectStr "TOKEN", MP.ObjectStr token)])
        , (MP.ObjectStr "META", MP.ObjectMap V.empty)
        , (MP.ObjectStr "PAYL", mpSerialize cPayload)
        ]
        where Authentication {..} = cAuthentication

instance MPSerializable a => Serializable (ClientMessage a) where
    serialize = MP.pack . mpSerialize

instance MPSerializable MP.Object where
    mpSerialize = id

instance MPSerializable Display where
    mpSerialize = MP.ObjectBin . BL.toStrict . serialize

-- Creates a display request.
displayRequest :: Authentication -> Display -> ClientMessage Display
displayRequest auth disp = ClientRequest
    { cReqId = 0
    , cVerb = "PUT"
    , cAuthentication = auth
    , cPayload = disp
    }

-- Creates a request for controller stream input.
controllerStreamRequest :: Authentication -> ClientMessage MP.Object
controllerStreamRequest auth = ClientRequest
    { cReqId = -1
    , cVerb = "STREAM"
    , cAuthentication = auth
    , cPayload = MP.ObjectNil
    }

-- * Server -> Client messages

data ServerMessage a = ServerRequest { sReqId :: Int, sPayload :: a }
                     | ServerError { sError :: T.Text }

class MPDeserializable a where
    -- | Converts from a MessagePack representation.
    mpDeserialize :: MP.Object -> Maybe a

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

instance MPDeserializable MP.Object where
    mpDeserialize = Just

instance MPDeserializable a => MPDeserializable [a] where
    mpDeserialize (MP.ObjectArray a) = mapM mpDeserialize $ V.toList a
    mpDeserialize _ = Nothing

instance MPDeserializable KeyEvent where
    mpDeserialize (MP.ObjectMap vo) = do
        let o = V.toList vo
        src <- MP.fromObject =<< lookup (MP.ObjectStr "src") o
        key <- MP.fromObject =<< lookup (MP.ObjectStr "key") o <|> lookup (MP.ObjectStr "btn") o
        dwn <- MP.fromObject =<< lookup (MP.ObjectStr "dwn") o
        return $ KeyEvent { eventSource = src, eventKey = key, eventPressed = dwn }
    mpDeserialize _ = Nothing

instance MPDeserializable a => Deserializable (ServerMessage a) where
    deserialize = mpDeserialize <=< MP.unpack
