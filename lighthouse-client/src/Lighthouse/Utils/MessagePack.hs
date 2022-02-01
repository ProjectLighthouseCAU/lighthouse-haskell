{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Lighthouse.Utils.MessagePack
    ( -- * Convenience functions for construction
      mpStr, mpInt, mpBool, mpMap, mpArray, mpBin, mpNil
      -- * Convenience functions for deconstruction
    , mpUnStr, mpUnInt, mpUnBool
      -- * Conversions to and from MessagePack
    , MPSerializable (..), MPDeserializable (..)
    ) where

import Control.Monad ((<=<))
import qualified Data.ByteString.Lazy as BL
import qualified Data.MessagePack as MP
import qualified Data.Text as T
import qualified Data.Vector as V
import Lighthouse.Utils.General ((<.$>))
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
mpUnStr :: MP.Object -> Maybe T.Text
mpUnStr (MP.ObjectStr t) = Just t
mpUnStr _ = Nothing

-- | Deconstructs a MessagePack integer.
mpUnInt :: MP.Object -> Maybe Int
mpUnInt (MP.ObjectInt n) = Just n
mpUnInt _ = Nothing

-- | Deconstructs a MessagePack boolean.
mpUnBool :: MP.Object -> Maybe Bool
mpUnBool (MP.ObjectBool b) = Just b
mpUnBool _ = Nothing

class MPSerializable a where
    -- | Converts to a MessagePack representation.
    mpSerialize :: a -> MP.Object

instance MPSerializable MP.Object where
    mpSerialize = id

class MPDeserializable a where
    -- | Converts from a MessagePack representation.
    mpDeserialize :: MP.Object -> Maybe a

instance MPDeserializable () where
    mpDeserialize _ = Just () -- we don't care about the result

instance MPDeserializable MP.Object where
    mpDeserialize = Just

instance MPDeserializable a => MPDeserializable [a] where
    mpDeserialize (MP.ObjectArray a) = mapM mpDeserialize $ V.toList a
    mpDeserialize _ = Nothing
