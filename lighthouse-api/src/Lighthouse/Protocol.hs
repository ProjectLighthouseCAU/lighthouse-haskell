{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Lighthouse.Protocol where

import qualified Data.ByteString.Lazy as BL
import qualified Data.MessagePack as MP
import qualified Data.Vector as V
import Lighthouse.Authentication
import Lighthouse.Display
import Lighthouse.Utils

data DisplayRequest = DisplayRequest { display :: Display, authentication :: Authentication }

instance Serializable DisplayRequest where
    serialize DisplayRequest {..} = MP.pack $ MP.ObjectMap $ V.fromList [(MP.ObjectStr "REID", MP.ObjectInt 0),
                                                                         (MP.ObjectStr "VERB", MP.ObjectStr "PUT"),
                                                                         (MP.ObjectStr "PATH", MP.ObjectArray $ V.fromList [MP.ObjectStr "user",
                                                                                                                            MP.ObjectStr username,
                                                                                                                            MP.ObjectStr "model"]),
                                                                         (MP.ObjectStr "AUTH", MP.ObjectMap $ V.fromList [(MP.ObjectStr "USER", MP.ObjectStr username),
                                                                                                                          (MP.ObjectStr "TOKEN", MP.ObjectStr token)]),
                                                                         (MP.ObjectStr "META", MP.ObjectMap V.empty),
                                                                         (MP.ObjectStr "PAYL", MP.ObjectBin $ BL.toStrict $ serialize display)]
        where Authentication {..} = authentication
