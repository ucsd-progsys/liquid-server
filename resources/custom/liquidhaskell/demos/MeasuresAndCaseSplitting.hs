module MeasuresAndCaseSplitting where

-- We define a data type with three alternatives

data ABC = A | B | C 


-- We define a measure over ABC and check how 
-- Liquid Haskell learns the value of the measure 
-- via case splitting

{-@ measure toInt @-}
toInt :: ABC -> Int 
toInt A = 1 
toInt B = 2
toInt C = 3 

-- When an ABC x value is not case analyzed, 
-- Liquid Haskell knows nothing of `toInt x`

{-@ unsafe :: x:ABC -> {o:() | 0 <= toInt x } @-}
unsafe     :: ABC -> () 
unsafe x   = ()

-- When an ABC x value is case analyzed, 
-- Liquid Haskell knows everything of `toInt x`

{-@ safe :: x:ABC -> {o:() | 0 <= toInt x } @-}
safe     :: ABC -> ()
safe A   = () 
safe B   = () 
safe C   = () 

-- When an ABC x value is partially case analyzed, 
-- Liquid Haskell knows everything of `toInt x` by default.
-- But, when no-case-expand flag in on, Liquid Haskell 
-- does not know the value of toInt x for the default cases.

{-@ LIQUID "--no-case-expand" @-}
{-@ safeBut :: x:ABC -> {o:() | 0 <= toInt x } @-}
safeBut     :: ABC -> ()
safeBut A   = () 
safeBut _   = () 

 
