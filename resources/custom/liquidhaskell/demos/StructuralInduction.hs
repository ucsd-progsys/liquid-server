
{-@ LIQUID "--higherorder"     @-}
{-@ LIQUID "--totality"        @-}
{-@ LIQUID "--exact-data-cons" @-}

module StructuralInduction where
import Language.Haskell.Liquid.ProofCombinators

import Prelude hiding (length)

length        :: List a -> Int 

(<>)          :: List a -> List a -> List a 
empty         :: List a 

leftId        :: List a -> Proof 
rightId       :: List a -> Proof 
associativity :: List a -> List a -> List a -> Proof 

-- Reflect A Recursive Data Type into Logic

data List a = N | C a (List a)

{-@ data List [length] a = N | C {hd :: a, tl :: List a} @-}

{-@ measure length            @-}
{-@ length      :: List a -> Nat @-}
length N        = 0 
length (C x xs) = 1 + length xs 

-- Definition And Reflection of the Monoid Operators

{-@ reflect empty @-}
empty  = N 

{-@ infix   <> @-}
{-@ reflect <> @-}
N        <> ys = ys 
(C x xs) <> ys = C x (xs <> ys)



-- Proving the Monoid Laws

{-@ leftId  :: x:List a -> { empty <> x == x } @-}
leftId x 
   =   empty <> x 
   ==. N <> x 
   ==. x 
   *** QED 

{-@ rightId  :: x:List a -> { x <> empty  == x } @-}
rightId N
   =   N <> empty 
   ==. N 
   *** QED 
rightId (C x xs)
   =   (C x xs) <> empty
   ==. C x (xs <> empty)
   ==. C x xs        ∵ rightId xs 
   *** QED 


{-@ associativity :: x:List a -> y:List a -> z:List a 
                  -> { x <> (y <> z) == (x <> y) <> z } @-}
associativity N y z 
  =   N <> (y <> z)
  ==. y <> z  
  ==. (N <> y) <> z 
  *** QED 
associativity (C x xs) y z 
  =  (C x xs) <> (y <> z)
  ==. C x (xs <> (y <> z))
  ==. C x ((xs <> y) <> z) ∵ associativity xs y z 
  ==. (C x (xs <> y)) <> z
  ==. ((C x xs) <> y) <> z 
  *** QED 
