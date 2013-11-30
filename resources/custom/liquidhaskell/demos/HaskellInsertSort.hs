module InsertSort where

infixr `C`
data L a = N | C a (L a)

{-@ data L a = N
             | C (x :: a) (xs :: L {v:a | x <= v})  @-}

{-@ insert :: Ord a => a -> L a -> L a @-}
insert :: Ord a => a -> L a -> L a
insert y (x `C` xs) | y <= x    = y `C` insert x xs
                    | otherwise = x `C` insert y xs

{-@ insertSort :: Ord a => [a] -> L a @-}
insertSort :: Ord a => [a] -> L a
insertSort = foldr insert N
