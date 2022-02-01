{-# LANGUAGE OverloadedStrings, FlexibleInstances, UndecidableInstances #-}
module Lighthouse.Utils.MessagePack
    ( -- * Convenience functions for construction
      mpStr, mpInt, mpBool, mpMap, mpArray, mpBin, mpNil
      -- * Convenience functions for deconstruction
    , mpUnStr, mpUnInt, mpUnBool, mpUnArray, mpUnMap, mpLookup
      -- * Conversions to and from MessagePack
    , MPSerializable (..), MPDeserializable (..)
    ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.MessagePack as MP
import qualified Data.Text as T
import qualified Data.Vector as V
import Lighthouse.Utils.General ((<.$>), maybeToRight)
import Lighthouse.Utils.Serializable

-- | Creates a MessagePack string.
mpStr :: T.Text -> MP.Object
mpStr = MP.ObjectStr

-- | Creates a MessagePack integer.
mpInt :: Int -> MP.Object
mpInt = MP.ObjectInt

-- | Creates a MessagePack boolean.
mpBool :: Bool -> MP.Object
mpBool = MP.ObjectBool

-- | Creates a MessagePack map.
mpMap :: [(T.Text, MP.Object)] -> MP.Object
mpMap = MP.ObjectMap . V.fromList . ((MP.ObjectStr <.$>) <$>)

-- | Creates a MessagePack array.
mpArray :: [MP.Object] -> MP.Object
mpArray = MP.ObjectArray . V.fromList

-- | Creates a MessagePack binary.
mpBin :: BL.ByteString -> MP.Object
mpBin = MP.ObjectBin . BL.toStrict

-- | Creates a MessagePack nil.
mpNil :: MP.Object
mpNil = MP.ObjectNil

-- | Deconstructs a MessagePack string.
mpUnStr :: MP.Object -> Either T.Text T.Text
mpUnStr (MP.ObjectStr t) = Right t
mpUnStr o = Left $ "Could not deconstruct " <> T.pack (show o) <> " as string"

-- | Deconstructs a MessagePack integer.
mpUnInt :: MP.Object -> Either T.Text Int
mpUnInt (MP.ObjectInt n) = Right n
mpUnInt o = Left $ "Could not deconstruct " <> T.pack (show o) <> " as int"

-- | Deconstructs a MessagePack boolean.
mpUnBool :: MP.Object -> Either T.Text Bool
mpUnBool (MP.ObjectBool b) = Right b
mpUnBool o = Left $ "Could not deconstruct " <> T.pack (show o) <> " as bool"

-- | Deconstructs a MessagePack array.
mpUnArray :: MP.Object -> Either T.Text [MP.Object]
mpUnArray (MP.ObjectArray v) = Right $ V.toList v
mpUnArray o = Left $ "Could not deconstruct " <> T.pack (show o) <> " as array"

-- | Deconstructs a MessagePack map.
mpUnMap :: MP.Object -> Either T.Text [(MP.Object, MP.Object)]
mpUnMap (MP.ObjectMap v) = Right $ V.toList v
mpUnMap o = Left $ "Could not deconstruct " <> T.pack (show o) <> " as map"

-- | Looks up a key in a deconstructed MessagePack map.
mpLookup :: T.Text -> [(MP.Object, MP.Object)] -> Either T.Text MP.Object
mpLookup key m = maybeToRight errMsg $ lookup (mpStr key) m
    where errMsg = "Could not find key " <> key <> " in " <> T.pack (show m)

class MPSerializable a where
    -- | Converts to a MessagePack representation.
    mpSerialize :: a -> MP.Object

instance MPSerializable MP.Object where
    mpSerialize = id

class MPDeserializable a where
    -- | Converts from a MessagePack representation.
    mpDeserialize :: MP.Object -> Either T.Text a

instance MPDeserializable () where
    mpDeserialize _ = Right () -- we don't care about the result

instance MPDeserializable MP.Object where
    mpDeserialize = Right

instance MPDeserializable a => MPDeserializable [a] where
    mpDeserialize (MP.ObjectArray a) = mapM mpDeserialize $ V.toList a
    mpDeserialize o = Left $ "Could not deserialize as array: " <> T.pack (show o)
