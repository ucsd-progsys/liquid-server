{-@ LIQUID "--no-termination" @-}

module Loop where
import Prelude hiding ((!!), length)
import Language.Haskell.Liquid.Prelude (liquidError)

(!)          :: L a -> Int -> a
length          :: L a -> Int


infixr `C`
data L a = N | C a (L a)

{-@ measure llen  :: L a -> Int
    llen (N)      = 0
    llen (C x xs) = 1 + (llen xs) @-}

{-@ type LtLen Xs = {v:Nat | v < (llen Xs)} @-}

{-@ (!)      :: xs:L a -> (LtLen xs) -> a @-}
(C x _)  ! 0 = x
(C _ xs) ! i = xs ! (i - 1)
_        ! _ = liquidError "never happens!"

{-@ length      :: xs:(L a) -> {v:Nat | v = (llen xs)} @-} 
length N        = 0
length (C _ xs) = 1 + length xs

-- | Higher Order Specifications

loop :: Int -> Int -> a -> (Int -> a -> a) -> a
loop lo hi base f = go lo base
  where go i acc | i < hi    = go (i+1) (f i acc)
                 | otherwise = acc

{-@ listNatSum :: L Nat -> Nat @-}
listNatSum     :: L Int -> Int
listNatSum xs  = loop 0 n 0 body 
  where body   = \i acc -> acc + (xs ! i)
        n      = length xs

{-@ type Even = {v:Int | v mod 2 =  0} @-}
{-@ type Odd  = {v:Int | v mod 2 /= 0} @-}

{-@ listEvenSum :: L Even -> Even @-}
listEvenSum     :: L Int -> Int
listEvenSum xs  = loop 0 n 0 body 
  where body   = \i acc -> acc + (xs ! i)
        n      = length xs

-- | Another Example

{-@ add :: n:Nat -> m:Nat -> {v:Int| v = m + n} @-}
add     :: Int -> Int -> Int
add n m = loop 0 m n (\_ i -> i + 1)
