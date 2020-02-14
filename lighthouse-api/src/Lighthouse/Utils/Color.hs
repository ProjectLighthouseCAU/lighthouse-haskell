module Lighthouse.Utils.Color where

-- | An RGB color.
data Color = Color Int Int Int

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
