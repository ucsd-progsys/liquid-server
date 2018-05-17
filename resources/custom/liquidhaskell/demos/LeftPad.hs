{-@ LIQUID "--reflection"  @-}
{-@ LIQUID "--ple"         @-}
{-@ infixr ++              @-}
{-@ infixr !!              @-}

module PadLeft where

import Prelude hiding (max, replicate, (++), (!!))
(!!) :: [a] -> Int -> a
size :: [a] -> Int
(++) :: [a] -> [a] -> [a]
leftPadObviousThm :: Int -> a -> [a] -> ()
thmReplicate      :: Int -> a -> Int -> ()
thmAppLeft        :: [a] -> [a] -> Int -> ()
thmAppRight       :: [a] -> [a] -> Int -> ()
thmLeftPad        :: Int -> a -> [a] -> Int -> ()

{-@ reflect max @-}
max :: Int -> Int -> Int
max x y = if x > y then x else y

-- A ghost function only used in the specification
{-@ leftPadVal :: n:{Int | False} -> _ -> _ -> _ -> _ @-}


-- Implementation
-- --------------

{-@ reflect leftPad @-}
leftPad :: Int -> a -> [a] -> [a]
leftPad n c xs
  | 0 < k     = replicate k c ++ xs
  | otherwise = xs
  where
    k         = n - size xs


{-@ measure size @-}
{-@ size :: [a] -> Nat @-}
size []     = 0
size (x:xs) = 1 + size xs


{-@ reflect ++ @-}
{-@ (++) :: xs:[a] -> ys:[a] -> {v:[a] | size v = size xs + size ys} @-}
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)


{-@ reflect replicate @-}
{-@ replicate :: n:Nat -> a -> {v:[a] | size v = n} @-}
replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n c = c : replicate (n - 1) c


-- What shall we Prove?
-- --------------------

-- My eyes roll whenever I read the phrase "proved X (a function, a program) _correct_".

-- There is no such thing as "correct".

-- There are only "specifications" or "properties",
-- and proofs that ensures that your code matches
-- those specifications or properties.

-- What, _specifications_ shall we verify about our
-- implementation of `leftPad`? One might argue that
-- the above code is "obviously correct", i.e. the
-- implementation more or less directly matches the
-- informal requirements.

-- Indeed, one way to formalize this "obviously correct"
-- is to verify a _specification_ that directly captures
-- the informal requirements:


{-@ leftPadObviousThm :: n:Int -> c:a -> xs:[a] ->
      { leftPad n c xs = if (size xs < n)
                         then (replicate (n - size xs) c ++ xs)
                         else xs }
  @-}
leftPadObviousThm _ _ _ = ()


-- Reasoning about Sequences
-- -------------------------

-- **Indexing into a Sequence**

{-@ reflect !! @-}
{-@ (!!) :: xs:[a] -> {n:Nat | n < size xs} -> a @-}
(x:_)  !! 0 = x
(_:xs) !! n = xs !! (n - 1)


-- **Replicated Sequences**

{-@ thmReplicate :: n:Nat -> c:a -> i:{Nat | i < n} ->
                    { replicate n c !! i  == c }
  @-}
thmReplicate n c i
  | i == 0    = ()
  | otherwise = thmReplicate (n - 1) c (i - 1)


-- **Concatenating Sequences**

{-@ thmAppLeft :: xs:[a] -> ys:[a] -> {i:Nat | i < size xs} ->
                  { (xs ++ ys) !! i == xs !! i }
  @-}
thmAppLeft (x:xs) ys 0 = ()
thmAppLeft (x:xs) ys i = thmAppLeft xs ys (i-1)

{-@ thmAppRight :: xs:[a] -> ys:[a] -> {i:Nat | size xs <= i} ->
                   { (xs ++ ys) !! i == ys !! (i - size xs) }
  @-}
thmAppRight []     ys i = ()
thmAppRight (x:xs) ys i = thmAppRight xs ys (i-1)


-- Proving Hillel's Specifications
-- -------------------------------

-- **Size Specification**

{-@ leftPad :: n:Int -> c:a -> xs:[a] ->
                {res:[a] | size res = max n (size xs)}
  @-}


-- **Pad-Value Specification**

{-@ thmLeftPad
      :: n:_ -> c:_ -> xs:{size xs < n} -> i:{Nat | i < n} ->
         { leftPad n c xs !! i ==  leftPadVal n c xs i }
  @-}

{-@ reflect leftPadVal @-}
leftPadVal n c xs i
  | i < k     = c
  | otherwise = xs !! (i - k)
  where k     = n - size xs


-- **Pad-Value Verification**

thmLeftPad n c xs i
  | i < k     = thmAppLeft  cs xs i `seq` thmReplicate k c i
  | otherwise = thmAppRight cs xs i
  where
    k         = n - size xs
    cs        = replicate k c






-- LINKS
--
-- [demo]:             http://goto.ucsd.edu:8090/index.html#?demo=LeftPad.hs
-- [dafny-leftpad]:    https://rise4fun.com/Dafny/nbNTl
-- [spark-leftpad]:    https://blog.adacore.com/taking-on-a-challenge-in-spark
-- [fstar-leftpad]:    https://gist.github.com/graydon/901f98049d05db65d9a50f741c7f7626
-- [idris-leftpad]:    https://github.com/hwayne/lets-prove-leftpad/blob/master/idris/Leftpad.idr
-- [dafny-seq-axioms]: https://github.com/Microsoft/dafny/blob/master/Binaries/DafnyPrelude.bpl#L898-L1110
-- [tag-reflection]:   /tags/reflection.html
-- [tag-ple]:          /tags/ple.html
