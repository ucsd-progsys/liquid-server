module InsertSort where

infixr `C`
data L a = N | C a (L a)

{-@ data L a = N
             | C (x :: a) (xs :: L {v:a | x <= v})  @-}

okList  = 0 `C` 1 `C` 2 `C` N

badList = 0 `C` 2 `C` 1 `C` N


{-@ insert :: Ord a => a -> L a -> L a @-}
insert :: Ord a => a -> L a -> L a
insert y (x `C` xs) 
  | y <= x    = y `C` (x `C` xs)
  | otherwise = x `C` insert y xs
insert y N    = y `C` N    


{-@ insertSort :: Ord a => [a] -> L a @-}
insertSort :: Ord a => [a] -> L a
insertSort = foldr insert N
