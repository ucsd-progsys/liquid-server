{-@ LIQUID "--higherorder"     @-}
{-@ LIQUID "--totality"        @-}

module RefinementReflection where
import Language.Haskell.Liquid.ProofCombinators

fib :: Int -> Int
propPlusAccum :: Int -> Int -> Proof 
propOnePlueOne :: () -> Proof 
fibTwo :: () -> Proof 
fibCongruence :: Int -> Int -> Proof
fibUp :: Int -> Proof 
fibTwoPretty :: () -> Proof 
fibThree :: () -> Proof 
fMono :: (Int -> Int)
      -> (Int -> Proof)
      -> Int
      -> Int 
      -> Proof 
fibMono :: Int -> Int -> Proof 


-- | Shallow Specifications

{-@ fib :: i:Nat -> Nat / [i] @-}
fib i | i == 0    = 0 
      | i == 1    = 1 
      | otherwise = fib (i-1) + fib (i-2)

-- | Propositions

{-@ type OnePlusOne = {v: Proof | 1 + 1 == 2} @-}

{-@ type OnePlusOne' = { 1 + 1 == 2} @-}

{-@ type PlusAccum = x:Int -> y:Int -> {x + y == y + x} @-} 




-- | Proofs

{-@ propOnePlueOne :: _ -> OnePlusOne @-} 
propOnePlueOne _ = trivial *** QED 

{-@ propPlusAccum :: PlusAccum @-} 
propPlusAccum _ _ = trivial *** QED 


-- | Refinement Reflection 

{-@ reflect fib @-}

-- | Step 1: Definition 

{-@ fibCongruence :: i:Nat -> j:Nat -> {i == j => fib i == fib j} @-}
fibCongruence _ _ = trivial *** QED 


-- | Step 2: Reflection

-- | Step 3: Application 

{-@ fibTwo :: _ -> { fib 2 == 1 } @-}
fibTwo _ = [fib 0, fib 1, fib 2] *** QED


-- | Structuring Pretty Proofs 

{-@ fibTwoPretty :: _ -> { fib 2 == 1 } @-}
fibTwoPretty _ 
  =   fib 2 
  ==. fib 1 + fib 0 
  *** QED


-- | Because operator 

{-@ fibThree :: _ -> { fib 3 == 2 } @-}
fibThree _ 
  =   fib 3 
  ==. fib 2 + fib 1
  ==. 1     + 1      ∵ fibTwoPretty ()
  ==. 2 
  *** QED


-- | Pencil & Paper Proofs by Induction 

{-@ fibUp :: i:Nat -> {fib i <= fib (i+1)} @-}
fibUp i
  | i == 0
  =   fib 0 <. fib 1
  *** QED
  | i == 1
  =   fib 1 <=. fib 1 + fib 0 <=. fib 2
  *** QED
  | otherwise
  =   fib i
  ==. fib (i-1) + fib (i-2)
  <=. fib i     + fib (i-2) ∵ fibUp (i-1)
  <=. fib i     + fib (i-1) ∵ fibUp (i-2)
  <=. fib (i+1)
  *** QED

-- | Higher Order Theorems

{-@ fMono :: f:(Nat -> Int)
          -> fUp:(z:Nat -> {f z <= f (z+1)})
          -> x:Nat
          -> y:{Nat|x < y}
          -> {f x <= f y} / [y] @-}
fMono f thm x y  
  | x + 1 == y
  = f y ==. f (x + 1)
         >. f x       ∵ thm x
        *** QED

  | x + 1 < y
  = f x
  <.  f (y-1)         ∵ fMono f thm x (y-1)
  <.  f y             ∵ thm (y-1)
  *** QED

{-@ fibMono :: n:Nat -> m:{Nat | n < m }  -> {fib n <= fib m} @-}
fibMono = fMono fib fibUp
