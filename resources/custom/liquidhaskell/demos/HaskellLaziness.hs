module Laziness where
import Language.Haskell.Liquid.Prelude


unsafeIncr :: Int -> [Int]
unsafeIncr x 
  -- unsafeIncr :: {v:Int| v < x} -> [Int]
  = x : unsafeIncr (x+1)

prop1 = liquidAssert ((\_ -> 0==1) unsafe)
  where unsafe = unsafeIncr 0


{-@ Lazy safeIncr @-}
{-@ safeIncr :: Int -> [Int] @-}
safeIncr :: Int -> [Int]
safeIncr x 
  = x : safeIncr (x+1)

prop2 = liquidAssert ((\_ -> 0==1) safe)
  where safe = safeIncr 0
