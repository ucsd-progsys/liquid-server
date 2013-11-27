module AbstractRefinements where

import Language.Haskell.Liquid.Prelude


data G = G { y:: Int}

{-@ data G <p :: Int -> Prop>= G ( y:: Int<p>) @-}

bar = let zero = 0 in
      let g    = G zero in
      case g of 
      G x -> liquidAssert (x >= 0)


-- | Lists
infixr `C`
data IL a = IN | IC {ix::a, ixs:: IL a}
{-@ data IL a = IN | IC (ix::a) (ixs:: IL {v:a| ix <= v}) @-}

data L a = N | C {x::a, xs::L a}

{-@ data L a <p :: a -> a -> Prop> = 
      N 
    | C (x::a) (xs:: L <p> a<p x>)    @-}

{-@ type IncrL a = L <{\hd v -> hd <= v}> a @-}

{-@ incrList' :: IncrL Int @-}
incrList' :: L Int
incrList' = 1 `C` 2 `C` 3 `C` N


incrList, decrList, diffList :: [Int]

{-@ type  IncrList a = [a]<{\hd v -> hd <= v}> @-}
{-@ incrList :: IncrList Int @-}
incrList = [1, 2, 3]

{-@ type  DecrList a = [a]<{\hd v -> hd >= v}> @-}
{-@ decrList :: DecrList Int @-}
decrList = [3, 2, 1]

{-@ type  DiffList a = [a]<{\hd v -> hd != v}> @-}
{-@ diffList :: DiffList Int @-}
diffList = [1, 8, 5]
