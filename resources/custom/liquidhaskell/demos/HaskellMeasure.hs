module SimpleRefinements where
import Prelude hiding ((!!), length)
import Language.Haskell.Liquid.Prelude

infixr `C`
data L a = N | C a (L a)

{-@ measure llen :: (L a) -> Int
    llen(N)      = 0
    llen(C x xs) = 1 + (llen xs)
  @-}


{-@ length :: xs:(L a) -> {v:Int | v = (llen xs)} @-}
length     :: L a -> Int
length N        = 0
length (C _ xs) = 1 + (length xs)


{-@ !! :: ls:(L a) -> {v:Nat | v < (llen ls)} -> a @-}

(!!)       :: L a -> Int -> a
(C x _) !!0 = x
(C _ xs)!!n = xs!!(n-1)
_       !!_ = liquidError "This should not happen!"


a = (1 `C` 2 `C` 3 `C` 4 `C` N) !! 3

-- | More measures

{-@ measure isNull :: L a -> Prop
    isNull (N) = true
    isNull (C x xs) = false
  @-}


cc = C 
nn = N
