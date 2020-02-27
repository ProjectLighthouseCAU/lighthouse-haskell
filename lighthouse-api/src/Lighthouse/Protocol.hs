{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Lighthouse.Protocol where

import qualified Data.ByteString.Lazy as BL
import qualified Data.MessagePack as MP
import qualified Data.Vector as V
import Lighthouse.Authentication
import Lighthouse.Display
import Lighthouse.Utils.Serializable

data Request a = Request { reqId :: Int, verb :: String, authentication :: Authentication, payload :: a }

class MPSerializable a where
    -- | Converts to a MessagePack representation.
    mpSerialize :: a -> MP.Object

instance MPSerializable a => MPSerializable (Request a) where
    mpSerialize Request {..} = MP.ObjectMap $ V.fromList [(MP.ObjectStr "REID", MP.ObjectInt 0),
                                                          (MP.ObjectStr "VERB", MP.ObjectStr "PUT"),
                                                          (MP.ObjectStr "PATH", MP.ObjectArray $ V.fromList [MP.ObjectStr "user",
                                                                                                             MP.ObjectStr username,
                                                                                                             MP.ObjectStr "model"]),
                                                          (MP.ObjectStr "AUTH", MP.ObjectMap $ V.fromList [(MP.ObjectStr "USER", MP.ObjectStr username),
                                                                                                           (MP.ObjectStr "TOKEN", MP.ObjectStr token)]),
                                                          (MP.ObjectStr "META", MP.ObjectMap V.empty),
                                                          (MP.ObjectStr "PAYL", mpSerialize payload)]
        where Authentication {..} = authentication

instance MPSerializable a => Serializable (Request a) where
    serialize = MP.pack . mpSerialize

instance MPSerializable MP.Object where
    mpSerialize = id

instance MPSerializable Display where
    mpSerialize = MP.ObjectBin . BL.toStrict . serialize

-- Creates a display request.
displayRequest :: Authentication -> Display -> Request Display
displayRequest auth disp = Request { reqId = 0, verb = "PUT", authentication = auth, payload = disp }

-- Creates a request for controller stream input.
controllerStreamRequest :: Authentication -> Request MP.Object
controllerStreamRequest auth = Request { reqId = -1, verb = "STREAM", authentication = auth, payload = MP.ObjectNil }
