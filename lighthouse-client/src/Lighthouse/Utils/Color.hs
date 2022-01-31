module Lighthouse.Utils.Color where

import qualified Data.ByteString.Lazy as BL
import Lighthouse.Utils.Serializable
import System.Random

-- | An RGB color.
data Color = Color Int Int Int
    deriving (Show, Eq)

instance Random Color where
    random gen = (Color r g b, gen3)
        where (r, gen1) = next gen
              (g, gen2) = next gen1
              (b, gen3) = next gen2
    
    randomR (Color lr lg lb, Color hr hg hb) gen = (Color r g b, gen3)
        where (r, gen1) = randomR (lr, hr) gen
              (g, gen2) = randomR (lg, hg) gen1
              (b, gen3) = randomR (lb, hb) gen2

instance Serializable Color where
    serialize (Color r g b) = BL.pack [r', g', b']
        where r' = fromIntegral r
              g' = fromIntegral g
              b' = fromIntegral b

black :: Color
black = Color 0 0 0

gray :: Color
gray = Color 128 128 128

white :: Color
white = Color 255 255 255

red :: Color
red = Color 255 0 0

green :: Color
green = Color 0 255 0

blue :: Color
blue = Color 0 0 255

yellow :: Color
yellow = Color 255 255 0

magenta :: Color
magenta = Color 255 0 255

cyan :: Color
cyan = Color 0 255 255
