module GCD where

import Prelude hiding (gcd, mod)

fibOk  :: Int -> Int
fibBad :: Int -> Int

{-@ fib   :: Int -> Int @-}
fibBad 0  = 1
fibBad n  = fibBad (n-1) + fibBad (n-2)

-- Can you figure out the problem?


{-@ mod :: a:Nat -> b:{v:Nat| ((v < a) && (v > 0))} -> {v:Nat | v < b} @-}
mod :: Int -> Int -> Int
mod a b
  | a < b = a
  | otherwise = mod (a - b) b

{-@ gcd :: a:Nat -> b:{v:Nat | v < a} -> Int @-}
gcd :: Int -> Int -> Int
gcd a 0 = a
gcd a b = gcd b (a `mod` b)
