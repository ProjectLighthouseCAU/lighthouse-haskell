{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Lighthouse.Display (
    lighthouseRows, lighthouseCols,
    emptyDisplay, coloredDisplay,
    Color (..),
    Display
) where

import Control.Monad (join)
import qualified Data.ByteString.Lazy as BL
import Lighthouse.Utils.Color
import Lighthouse.Utils.Serializable

-- | The lighthouse's width in pixels.
lighthouseCols :: Int
lighthouseCols = 28

-- | The lighthouse's height in pixels.
lighthouseRows :: Int
lighthouseRows = 14

-- | A representation of the lighthouse's pixels.
type Display = [[Color]]

-- | A black display.
emptyDisplay :: Display
emptyDisplay = coloredDisplay black

-- | A display with a uniformly colored background.
coloredDisplay :: Color -> Display
coloredDisplay c = (const (const c <$> [0..lighthouseCols]) <$> [0..lighthouseRows])

instance Serializable Color where
    serialize (Color r g b) = BL.pack [r', g', b']
        where r' = fromIntegral r
              g' = fromIntegral g
              b' = fromIntegral b

-- | Converts a display to a binary representation.
instance Serializable Display where
    serialize = BL.concat . (serialize <$>) . join
