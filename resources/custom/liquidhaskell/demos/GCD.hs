module GCD where

import Prelude hiding (gcd, mod)

{-@ mod :: a:Nat -> b:{v:Nat| 0 < v} -> {v:Nat | v < b} @-}
mod :: Int -> Int -> Int
mod a b
  | a < b = a
  | otherwise = mod (a - b) b

{-@ gcd :: a:Nat -> b:{v:Nat | v < a} -> Int @-}
gcd :: Int -> Int -> Int
gcd a 0 = a
gcd a b = gcd b (a `mod` b)
