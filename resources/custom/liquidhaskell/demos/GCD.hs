{-@ LIQUID "--short-names" @-}

module GCD where

import Prelude hiding (gcd, mod)

mod :: Int -> Int -> Int
gcd :: Int -> Int -> Int

{-@ mod :: a:Nat -> b:{v:Nat| 0 < v} -> {v:Nat | v < b} @-}
mod a b
  | a < b = a
  | otherwise = mod (a - b) b

{-@ gcd :: a:Nat -> b:{v:Nat | v < a} -> Int @-}
gcd a 0 = a
gcd a b = gcd b (a `mod` b)



