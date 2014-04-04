module SimpleRefinements where
import Prelude hiding ((!!), length)
import Language.Haskell.Liquid.Prelude

zero, zero', one :: Int

{-@ type EqZero = {v:Int | v = 0} @-}

{-@ zero :: EqZero @-}
zero = 0

{-@ zero' :: Nat @-}
zero'     = zero

{-@ one :: Nat @-}
one = 1

infixr `C`
data L a = N | C a (L a)

{-@ natList :: L Nat @-}
natList     :: L Int
natList     =  0 `C` 1 `C` 3 `C` N


-- | Dependent Functions

{-@ safeDiv :: Int -> {v:Int | v != 0} -> Int @-}
safeDiv     :: Int -> Int -> Int
safeDiv x y = x `div` y

m = choose 0
n = 4 `safeDiv` 2 

