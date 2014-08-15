module Filter3 where

-- Dump plain sigs here.

import Prelude hiding (filter)
isNat, isEven, isOdd :: Int -> Maybe Int
filter :: (a -> Maybe a) -> [a] -> [a]

-----------------------------------------------------------------
-- | Preliminaries 
-----------------------------------------------------------------

{-  type Nat  = {v:Int | 0 <= v}        -}
{-@ type Even = {v:Int | v mod 2 == 0} @-}
{-@ type Odd  = {v:Int | v mod 2 /= 0} @-}

{-@ isNat          :: Int -> Maybe Nat @-}
isNat x
  | x >= 0         = Just x
  | otherwise      = Nothing

{-@ isEven         :: Int -> Maybe Even @-}
isEven x           
  | x `mod` 2 == 0 = Just x
  | otherwise      = Nothing

{-@ isOdd          :: Int -> Maybe Odd  @-}
isOdd x
  | x `mod` 2 == 1 = Just x
  | otherwise      = Nothing

-----------------------------------------------------------------
-- | Take 1: map, maybe
-----------------------------------------------------------------

filter1          :: (a -> Maybe b) -> [a] -> [b]
filter1 f []     = []
filter1 f (x:xs) = case f x of
                     Just y  -> y : filter1 f xs 
                     Nothing ->     filter1 f xs

{-@ getNats1 :: [Int] -> [Nat] @-}
getNats1     = filter1 isNat

{-@ getEvens1 :: [Int] -> [Even] @-}
getEvens1    = filter1 isEven

{-@ getOdds1 :: [Int] -> [Odd] @-}
getOdds1     = filter1 isOdd

-----------------------------------------------------------------
-- | Take 2: single type variable
-----------------------------------------------------------------

filter2          :: (a -> Maybe a) -> [a] -> [a]
filter2 f []     = []
filter2 f (x:xs) = case f x of
                     Just y  -> y : filter2 f xs 
                     Nothing ->     filter2 f xs

{-@ getNats2 :: [Int] -> [Nat] @-}
getNats2     = filter2 isNat

{-@ getEvens2 :: [Int] -> [Even] @-}
getEvens2    = filter2 isEven

{-@ getOdds2 :: [Int] -> [Odd] @-}
getOdds2     = filter2 isOdd

-----------------------------------------------------------------
-- | Take 3: abstractly refined filter 
-----------------------------------------------------------------

{-@ filter      :: forall a <p :: a -> Prop>.
                     (a -> Maybe a<p>) -> [a] -> [a<p>] @-}
filter f []     = []
filter f (x:xs) = case f x of
                    Just x'  -> x' : filter f xs 
                    Nothing ->       filter f xs

{-@ getNats :: [Int] -> [Nat] @-}
getNats     = filter isNat

{-@ getEvens :: [Int] -> [Even] @-}
getEvens    = filter isEven

{-@ getOdds :: [Int] -> [Odd] @-}
getOdds     = filter isOdd
