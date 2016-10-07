

{-@ LIQUID "--short-names" @-}
{-@ LIQUID "--no-termination" @-}

module Vectors ( Vector
               , map
               , append
               , reverse
               )
where

import Prelude hiding (reverse, map)

infixr 9 :+:

-- | A simple `Vector` data type from scratch:
data Vector a = Emp
              | (:+:) a (Vector a)
                deriving (Eq, Ord, Show)

{-@ measure size @-}
size :: Vector a -> Int
size Emp        = 0
size (x :+: xs) = 1 + size xs

{-@ invariant {v:Vector a | 0 <= size v} @-}

-- | Type alias for Vector of a given size N

{-@ type VectorN a N  = {v:Vector a | size v = N} @-}

---------------------------------------------------------------
-- | Append ---------------------------------------------------
---------------------------------------------------------------

{-@ append :: xs:Vector a -> ys:Vector a -> VectorN a {size xs + size ys} @-}
append Emp ys        = ys
append (x :+: xs) ys = x :+: append xs ys

---------------------------------------------------------------
-- | Map ------------------------------------------------------
---------------------------------------------------------------

{-@ map :: (a -> b) -> xs:Vector a -> VectorN b {size xs} @-}
map f Emp        = Emp
map f (x :+: xs) = f x :+: map f xs

---------------------------------------------------------------
-- | Reverse --------------------------------------------------
---------------------------------------------------------------

{-@ reverse :: xs:Vector a -> VectorN a {size xs} @-}
reverse xs            = go Emp xs
  where
    go acc Emp        = acc
    go acc (x :+: xs) =  go (x :+: acc) xs
