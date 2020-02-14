module Lighthouse.Utils.Random (nRandomsR, nRandoms) where

import System.Random

-- | Generates n random values in the given range without consuming the generator.
nRandomsR :: (RandomGen g, Random a) => Int -> ([a], [a]) -> g -> ([a], g)
nRandomsR 0 _ g     = ([], g)
nRandomsR n ((l:ls), (h:hs)) g = (x:xs, g'')
    where (x, g') = randomR (l, h) g
          (xs, g'') = nRandomsR (n - 1) (ls, hs) g'

-- | Generates n random values without consuming the generator.
nRandoms :: (RandomGen g, Random a) => Int -> g -> ([a], g)
nRandoms 0 g = ([], g)
nRandoms n g = (x:xs, g'')
    where (x, g') = random g
          (xs, g'') = nRandoms (n - 1) g'
