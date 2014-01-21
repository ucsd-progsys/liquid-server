module GCD where

import Prelude hiding (gcd, mod)


fibOk  :: Int -> Int
fibBad :: Int -> Int

{-@ fibBad    :: Int -> Int @-}
fibBad 0      = 1
fibBad 1      = 1
fibBad n      = fibBad (n-1) + fibBad (n-2)

{-@ fibOk :: Nat -> Nat @-}
fibOk n 
  | n <= 1    = 1
  | otherwise = fibOk (n-1) + fibOk (n-2)






{-@ mod :: a:Nat -> b:{v:Nat| ((v < a) && (v > 0))} -> {v:Nat | v < b} @-}
mod :: Int -> Int -> Int
mod a b | a - b >  b = mod (a - b) b
        | a - b <  b = a - b
        | a - b == b = 0

{-@ gcd :: a:Nat -> b:{v:Nat | v < a} -> Int @-}
gcd :: Int -> Int -> Int
gcd a 0 = a
gcd a b = gcd b (a `mod` b)
