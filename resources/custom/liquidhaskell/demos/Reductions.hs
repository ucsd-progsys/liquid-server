{-@ LIQUID "--higherorder"    @-}
{-@ LIQUID "--totalhaskell"   @-}
{-@ LIQUID "--exactdc"        @-}
{-@ LIQUID "--diffcheck"      @-}
{-@ LIQUID "--pruneunsorted"  @-}
{-@ LIQUID "--eliminate=some" @-}

module Reductions where

import Language.Haskell.Liquid.ProofCombinators
import Language.Haskell.Liquid.Reduction

geqZero  :: Peano -> Proof 
leqTotal :: Peano -> Peano -> Proof
toNat    :: Peano -> Int

geqZeroNat          :: Nat -> Proof 
geqZeroByReduction  :: Peano -> Proof 
leqTotalByReduction :: Peano -> Peano -> Proof 
leqTotalNat         :: Nat -> Nat -> Proof



-- | Properties of Peano Numbers

{-@ data Peano [toNat] = Z | S Peano @-}
data Peano = Z | S Peano deriving (Eq)

{-@ reflect leqPeano @-}
leqPeano :: Peano -> Peano -> Bool
leqPeano Z _         = True
leqPeano _ Z         = False
leqPeano (S n) (S m) = leqPeano n m

{-@ geqZero :: n:Peano -> {leqPeano Z n} @-}
geqZero n = leqPeano Z n *** QED 


{-@ leqTotal :: n:Peano -> m:Peano 
             -> {(leqPeano n m) || (leqPeano m n)} 
             /  [toNat n + toNat m] @-}
leqTotal Z m = leqPeano Z m *** QED
leqTotal n Z = leqPeano Z n *** QED
leqTotal (S n) (S m)
  =   (leqPeano (S n) (S m) || leqPeano (S m) (S n))
  ==. (leqPeano n m || leqPeano (S m) (S n)) 
      ? (leqTotal n m)
  ==. (leqPeano n m || leqPeano m n) 
      ? (leqTotal m n)
  *** QED


{-@ measure toNat @-}
{-@ toNat :: Peano -> Nat @-}
toNat Z     = 0
toNat (S n) = 1 + toNat n

{-@ type Nat = { n:Int | 0 <= n } @-}
type Nat     = Int

-- | Reduction of Operators 

{-@ reflect leqPeanoNat @-}
leqPeanoNat :: Peano -> Peano -> Bool 
leqPeanoNat n m = toNat n `leqInt` toNat m  

{-@ reflect leqInt @-}
leqInt :: Int -> Int -> Bool 
leqInt x y = x <= y

-- | Proof Reductions

{-@ geqZeroNat :: n:Nat -> {leqInt 0 n} @-}
geqZeroNat n = leqInt 0 n *** QED 

{-@ geqZeroByReduction :: n:Peano -> {leqPeanoNat Z n} @-}
geqZeroByReduction n 
  = leqPeanoNat Z n ==. True 
  ? reduction toNat geqZeroNat n 
  *** QED 


{-@ leqTotalNat :: n:Nat -> m:Nat -> { leqInt n m || leqInt m n } @-}
leqTotalNat n m = (leqInt n m || leqInt m n) *** QED 

{-@ leqTotalByReduction :: n:Peano -> m:Peano
   -> { leqPeanoNat n m || leqPeanoNat m n } @-}
leqTotalByReduction n m 
  = (leqPeanoNat n m || leqPeanoNat m n) ==. True 
  ? reduction2 toNat leqTotalNat n m  
  *** QED 
