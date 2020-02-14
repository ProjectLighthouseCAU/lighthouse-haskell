module Lighthouse.Display where

import Control.Monad (join)
import qualified Data.ByteString as B

-- | An RGB color.
data Color = Color Int Int Int

-- | A representation of the lighthouse's pixels.
type Display = [[Color]]

-- | Converts a color to a binary representation.
serializeColor :: Color -> B.ByteString
serializeColor (Color r g b) = B.pack [r', g', b']
    where r' = fromIntegral r
          g' = fromIntegral g
          b' = fromIntegral b

-- | Converts a display to a binary representation.
serializeDisplay :: Display -> B.ByteString
serializeDisplay = B.concat . (serializeColor <$>) . join
