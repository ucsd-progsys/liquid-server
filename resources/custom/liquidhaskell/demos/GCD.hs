module GCD where

import Prelude hiding (gcd, mod)

{-@ fib  :: Nat -> Nat @-}
fib 0    = 1
fib n    = fibBad (n-1) + fibBad (n-2)

-- Eh, can you figure out the problem?


{-@ mod :: a:Nat -> b:{v:Nat| ((v < a) && (v > 0))} -> {v:Nat | v < b} @-}
mod :: Int -> Int -> Int
mod a b | a - b >  b = mod (a - b) b
        | a - b <  b = a - b
        | a - b == b = 0

{-@ gcd :: a:Nat -> b:{v:Nat | v < a} -> Int @-}
gcd :: Int -> Int -> Int
gcd a 0 = a
gcd a b = gcd b (a `mod` b)
