module Lighthouse.Utils.General
    ( fst3, snd3, thd3
    , liftMaybe
    , (<.$>), (<$.>), (<.$.>)
    , whileM_
    ) where

import Control.Monad.Trans.Maybe

-- | Lifts an optional value into the maybe transformer.
liftMaybe :: Monad m => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return

-- | Fetches the first element of a triple.
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

-- | Fetches the second element of a triple.
snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

-- | Fetches the third element of a triple.
thd3 :: (a, b, c) -> c
thd3 (_, _, z) = z

-- | Maps over the first element in a pair.
(<.$>) :: (a -> b) -> (a, c) -> (b, c)
f <.$> (x, y) = (f x, y)

-- | Maps over the second element in a pair.
(<$.>) :: (a -> b) -> (c, a) -> (c, b)
f <$.> (x, y) = (x, f y)

-- | Maps over both elements of a pair.
(<.$.>) :: (a -> b) -> (a, a) -> (b, b)
(<.$.>) f = (f <$.>) . (f <.$>)

-- | Loops while the given condition is true.
whileM_ :: Monad m => m Bool -> m a -> m ()
whileM_ cond body = do
    c <- cond
    if c then do
        body
        whileM_ cond body
    else return ()
