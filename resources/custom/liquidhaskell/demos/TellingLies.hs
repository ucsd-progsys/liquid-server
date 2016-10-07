{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--short-names"    @-}

module TellingLies where

import Language.Haskell.Liquid.Prelude (liquidError)

safeDiv  :: Int -> Int -> Int
foo     :: Int -> Int
explode :: Int

-- | Going Wrong 
{-@ safeDiv :: n:Int -> d:{v:Int | v /= 0} -> Int @-}
safeDiv n 0 = liquidError "Please don't safeDiv-by-zero"
safeDiv n d = n `div` d

{-@ foo :: n:Int -> {v:Nat | v < n} @-}
foo n | n > 0     = n - 1
      | otherwise = foo n


explode = let z = 0
              d = foo z
          in  
              (\x -> 2013 `safeDiv` z) d
