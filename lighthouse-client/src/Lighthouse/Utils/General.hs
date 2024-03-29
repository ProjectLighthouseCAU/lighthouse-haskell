module Lighthouse.Utils.General
    ( fst3, snd3, thd3
    , liftMaybe, maybeToRight, rightToMaybe
    , (<.$>), (<$.>), (<.$.>)
    , whileM_
    ) where

import Control.Monad (when)
import Control.Monad.Trans.Maybe

-- | Lifts an optional value into the maybe transformer.
liftMaybe :: Monad m => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return

-- | The right element or the given value.
maybeToRight :: b -> Maybe a -> Either b a
maybeToRight x Nothing  = Left x
maybeToRight _ (Just y) = Right y

-- | The right side of the either.
rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left _)  = Nothing
rightToMaybe (Right y) = Just y

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
    when c $ do
        _ <- body
        whileM_ cond body
