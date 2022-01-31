{-# LANGUAGE TupleSections #-}
module Lighthouse.Utils.Random
    ( RandomM (..)
    , runRandomMIO, getRandomGen, randomM, randomRM, nRandomsR, nRandoms
    ) where

import System.Random

newtype RandomM g a = RandomM { runRandomM :: g -> (a, g) }

instance Functor (RandomM g) where
    fmap f r = RandomM $ \g -> let (x, g') = runRandomM r g
                               in (f x, g')

instance Applicative (RandomM g) where
    pure x = RandomM (x,)
    rf <*> r = RandomM $ \g -> let (x, g') = runRandomM r g
                                   (f, g'') = runRandomM rf g'
                               in (f x, g'')

instance Monad (RandomM g) where
    r >>= f = RandomM $ \g -> let (x, g') = runRandomM r g
                              in runRandomM (f x) g'

-- | Runs a random monad using the global RNG.
runRandomMIO :: RandomM StdGen a -> IO a
runRandomMIO r = fst . runRandomM r <$> getStdGen

-- | Fetches the generator inside the random monad.
getRandomGen :: RandomM g g
getRandomGen = RandomM $ \g -> (g, g)

-- | Generates a random value inside the random monad.
randomM :: (RandomGen g, Random a) => RandomM g a
randomM = RandomM random

-- | Generates a bounded random value inside the random monad.
randomRM :: (RandomGen g, Random a) => (a, a) -> RandomM g a
randomRM r = RandomM $ randomR r

-- | Generates n random values in the given range without consuming the generator.
nRandomsR :: (RandomGen g, Random a) => Int -> ([a], [a]) -> RandomM g [a]
nRandomsR 0 _ = return []
nRandomsR n (l:ls, h:hs) = do
    x <- randomRM (l, h)
    (x:) <$> nRandomsR (n - 1) (ls, hs)
nRandomsR n _ = error $ "No range(s) for remaining " ++ show n ++ " random value(s)!"

-- | Generates n random values without consuming the generator.
nRandoms :: (RandomGen g, Random a) => Int -> RandomM g [a]
nRandoms 0 = return []
nRandoms n = do
    x <- randomM
    (x:) <$> nRandoms (n - 1)
