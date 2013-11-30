{-@ LIQUID "--no-termination" @-}

module TellingLies where

import Language.Haskell.Liquid.Prelude (liquidError)

divide  :: Int -> Int -> Int
foo     :: Int -> Int
explode :: Int

-- | Going Wrong 
{-@ divide :: n:Int -> d:{v:Int | v /= 0} -> Int @-}
divide n 0 = liquidError "no you didn't!"
divide n d = n `div` d

{-@ foo :: n:Int -> {v:Nat | v < n} @-}
foo n | n > 0     = n - 1
      | otherwise = foo n


explode = let z = 0
          in  (\x -> (2013 `divide` z)) (foo z)
