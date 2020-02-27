module Lighthouse.Utils.Serializable (Serializable (..), Deserializable (..)) where

import qualified Data.ByteString.Lazy as BL

class Serializable t where
    -- | Converts to a binary representation.
    serialize :: t -> BL.ByteString

class Deserializable t where
    -- | Converts from a binary representation.
    deserialize :: BL.ByteString -> Maybe t
