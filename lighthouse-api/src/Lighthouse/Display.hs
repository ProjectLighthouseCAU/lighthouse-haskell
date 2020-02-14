{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Lighthouse.Display (Color (..), Display) where

import Control.Monad (join)
import qualified Data.ByteString as B
import Lighthouse.Utils

-- | An RGB color.
data Color = Color Int Int Int

-- | A representation of the lighthouse's pixels.
type Display = [[Color]]

instance Serializable Color where
    serialize (Color r g b) = B.pack [r', g', b']
        where r' = fromIntegral r
              g' = fromIntegral g
              b' = fromIntegral b

-- | Converts a display to a binary representation.
instance Serializable Display where
    serialize = B.concat . (serialize <$>) . join
