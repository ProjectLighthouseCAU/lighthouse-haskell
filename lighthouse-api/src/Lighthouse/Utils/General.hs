module Lighthouse.Utils.General (fst3, snd3, thd3, (<.$>), (<$.>), (<.$.>)) where

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

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
