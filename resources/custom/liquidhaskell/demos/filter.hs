{-@ LIQUID "--short-names" @-}
{-# LANGUAGE ScopedTypeVariables #-}

module Filter (filter) where

import Prelude hiding (filter)
import Data.Set (Set)

import Prelude hiding (filter)
isPos, isEven, isOdd :: Int -> Maybe Int
filter, filter3      :: (a -> Maybe a) -> [a] -> [a]

{-@ measure elts :: [a] -> (Set a)
    elts ([])   = {v | Set_emp v }
    elts (x:xs) = {v | v = Set_cup (Set_sng x) (elts xs) }
  @-}

------------------------------------------------------------------

-- | Goal

{-@ getPoss  :: [Int] -> [Pos] @-}
getPoss      = filter isPos

{-@ getEvens :: [Int] -> [Even] @-}
getEvens     = filter isEven

{-@ getOdds  :: [Int] -> [Odd] @-}
getOdds      = filter isOdd

-- | `Pos`, `Even` and `Odd` are just subsets of `Int`:

{-@ type Pos  = {v:Int| 0 < v}        @-}

{-@ type Even = {v:Int| v mod 2 == 0} @-}

{-@ type Odd  = {v:Int| v mod 2 /= 0} @-}


--------------------------------------------------------------------------
-- | Take 1: Map, maybe?
--------------------------------------------------------------------------

filter1          :: (a -> Maybe b) -> [a] -> [b]

filter1 f []     = []
filter1 f (x:xs) = case f x of
                     Just y  -> y : filter1 f xs
                     Nothing ->     filter1 f xs

isPos x
  | x > 0          = Just x
  | otherwise      = Nothing

isEven x
  | x `mod` 2 == 0 = Just x
  | otherwise      = Nothing

isOdd x
  | x `mod` 2 /= 0 = Just x
  | otherwise      = Nothing


{-@ getPoss1 :: [Int] -> [Pos] @-}
getPoss1     = filter1 isPos

{-@ getEvens1 :: [Int] -> [Even] @-}
getEvens1    = filter1 isEven

{-@ getOdds1 :: [Int] -> [Odd] @-}
getOdds1     = filter1 isOdd


--------------------------------------------------------------------------
-- | Take 2: One Type Variable
--------------------------------------------------------------------------

filter2          :: (a -> Maybe a) -> [a] -> [a]

filter2 f []     = []
filter2 f (x:xs) = case f x of
                     Just y  -> y : filter2 f xs
                     Nothing ->     filter2 f xs

{-@ getPoss2 :: [Int] -> [Pos] @-}
getPoss2     = filter2 isPos

{-@ getEvens2 :: [Int] -> [Even] @-}
getEvens2    = filter2 isEven

{-@ getOdds2 :: [Int] -> [Odd] @-}
getOdds2     = filter2 isOdd

--------------------------------------------------------------------------
-- | Take 3: Add Abstract Refinement
--------------------------------------------------------------------------

{-@ filter3      :: forall a <p :: a -> Bool>.
                      (a -> Maybe a<p>) -> [a] -> [a<p>] @-}
filter3 f []     = []
filter3 f (x:xs) = case f x of
                     Just x'  -> x' : filter3 f xs
                     Nothing ->       filter3 f xs


--  Now, we've **decoupled** the filter-property from the type variable `a`.

-- The input still is a mere `a`, but the output is an `a` with bells on,
-- specifically, which satisfies the (abstract) refinement `p`.

-- Voila!


{-@ getPoss3  :: [Int] -> [Pos] @-}
getPoss3      = filter3 isPos

{-@ getEvens3 :: [Int] -> [Even] @-}
getEvens3     = filter3 isEven

{-@ getOdds3  :: [Int] -> [Odd] @-}
getOdds3      = filter3 isOdd

doubles       = filter3 (\x -> Just (x + x)) 


--------------------------------------------------------------------------

{-@ filter      :: forall a <p :: a -> Bool>.
                       (x:a -> Maybe {v:a<p> | v = x})
                    -> xs:[a]
                    -> {v:[a<p>] | Set_sub (elts v) (elts xs)} @-}

filter f []     = []
filter f (x:xs) = case f x of
                    Just x'  -> x' : filter f xs
                    Nothing ->       filter f xs
