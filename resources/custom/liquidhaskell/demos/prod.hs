{-@ LIQUID "--real"     @-}
{-@ LIQUID "--total" @-}


module Vectors where

import Prelude hiding (sum, product, zipWith)

flatten :: Int -> Int -> [[a]] -> [a]

-- | Lets reuse plain lists

type Vec a = [a]

-- | The `len` function is defined in the LH prelude as:
{-

   len        :: [a] -> Int
   len []     = 0
   len (_:xs) = 1 + len xs
-}

             
{-@ type VecN a N = {v : Vec a | len v = N} @-}

--------------------------------------------------------------------
-- | Product -----------------------------------------------------------
--------------------------------------------------------------------

{-@ product   :: xs:Vec a -> ys:Vec a -> VecN a {len xs * len ys} @-}
product xs ys = flatten n m $ map (\y -> map (* y) xs) ys
  where
    m         = length xs
    n         = length ys

{-@ flatten          :: n:Nat -> m:Nat -> VecN (VecN a m) n -> VecN a {m * n} @-}
flatten n m []       = []
flatten n m (xs:xss) = xs ++ flatten (n-1) m xss 


--------------------------------------------------------------------
-- | Sum -----------------------------------------------------------
--------------------------------------------------------------------

{-@ sum       :: xs:Vec a -> ys:VecN a {len xs}-> VecN a {len xs} @-}
sum xs ys     = zipWith (+) xs ys

{-@ zipWith             :: (a -> b -> c) -> xs:Vec a -> ys:VecN b {len xs} -> VecN c {len xs} @-}
zipWith f [] []         = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys


{-@ sum'       :: xs:Vec a -> ys:Vec a -> VecN a {len xs + len ys} @-}
sum' [] ys     = ys
sum' (x:xs) ys = x : sum' xs ys

--------------------------------------------------------------------
-- | Examples ------------------------------------------------------
--------------------------------------------------------------------

{-@ example1   :: b:VecN a 3 -> VecN a 6 @-}
example1 b     = ([1, 2] `product` b) `sum` [1, 2, 3, 4, 5, 6]
    
{-@ example2   :: a:Vec a -> b:{Vec a | len b * len a = 6} -> VecN _ 6 @-}
example2 a b   = (a `product` b) `sum` [1,2,3,4,5,6]

  

