module Lighthouse.Display
    ( lighthouseRows, lighthouseCols
    , emptyDisplay, coloredDisplay
    , Color (..), Display (..), Row (..)
    ) where

import Control.Monad (join)
import qualified Data.ByteString.Lazy as BL
import Lighthouse.Utils.Color
import Lighthouse.Utils.General
import Lighthouse.Utils.Serializable
import Lighthouse.Utils.Random
import System.Random

-- | The lighthouse's width in pixels.
lighthouseCols :: Int
lighthouseCols = 28

-- | The lighthouse's height in pixels.
lighthouseRows :: Int
lighthouseRows = 14

-- | A representation of the lighthouse's pixels.
newtype Row = Row [Color] deriving (Show, Eq)
newtype Display = Display [Row] deriving (Show, Eq)

-- | A black display.
emptyDisplay :: Display
emptyDisplay = coloredDisplay black

-- | A display with a uniformly colored background.
coloredDisplay :: Color -> Display
coloredDisplay c = Display $ Row (c <$ [0..lighthouseCols]) <$ [0..lighthouseRows]

rowToList :: Row -> [Color]
rowToList (Row xs) = xs

displayToList :: Display -> [Row]
displayToList (Display xs) = xs

instance Random Display where
    random    = runRandomM $ Display <$> nRandoms lighthouseRows
    randomR r = runRandomM $ Display <$> nRandomsR lighthouseRows (displayToList <.$.> r)

instance Random Row where
    random    = runRandomM $ Row <$> nRandoms lighthouseCols
    randomR r = runRandomM $ Row <$> nRandomsR lighthouseCols (rowToList <.$.> r)

-- | Converts a display to a binary representation.
instance Serializable Display where
    serialize (Display d) = BL.concat $ serialize <$> d

instance Serializable Row where
    serialize (Row r) = BL.concat $ serialize <$> r
