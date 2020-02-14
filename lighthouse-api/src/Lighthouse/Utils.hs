module Lighthouse.Utils (Serializable (..)) where

import qualified Data.ByteString as B

class Serializable t where
    -- | Converts to a binary representation.
    serialize :: t -> B.ByteString

class Deserializable t where
    -- | Converts from a binary representation.
    deserialize :: B.ByteString -> t
