module Lighthouse.Utils.Color where

import Lighthouse.Utils.General (fst3, snd3, thd3)
import System.Random

-- | An RGB color.
data Color = Color Int Int Int

instance Random Color where
    random gen = (Color r g b, gen3)
        where (r, gen1) = next gen
              (g, gen2) = next gen1
              (b, gen3) = next gen2
    
    randomR (Color lr lg lb, Color hr hg hb) gen = (Color r g b, gen3)
        where (r, gen1) = randomR (lr, hr) gen
              (g, gen2) = randomR (lg, hg) gen1
              (b, gen3) = randomR (lb, hb) gen2

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
