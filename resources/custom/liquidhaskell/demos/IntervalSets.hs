{-@ LIQUID "--short-names"    @-}
{-@ LIQUID "--exact-data-con" @-}
{-@ LIQUID "--no-adt"         @-}
{-@ LIQUID "--prune-unsorted" @-}
{-@ LIQUID "--higherorder"    @-}
{-@ LIQUID "--no-termination" @-}

module Intervals where

data Interval  = I
  { from :: Int
  , to   :: Int
  } deriving (Show)


-- Making Illegal Intervals Unrepresentable
-- ----------------------------------------

-- **A Single Interval**

-- We can ensure that each interval is **non-empty** by
-- refining the data type for a single interval to specify
-- that the `to` field must be strictly bigger than the `from`
-- field:


{-@ data Interval = I
      { from :: Int
      , to   :: {v: Int | from < v }
      }
  @-}


-- Now, LH will ensure that we can only construct *legal*,
-- non-empty `Interval`s

goodItv = I 10 20
badItv  = I 20 10     -- ILLEGAL: empty interval!


-- **Many Intervals**

-- We can represent arbitrary sets as a *list of* `Interval`s:

data Intervals = Intervals { itvs :: [Interval] }

-- First, we specify a *lower-bounded* `Interval` as:

{-@ type LbItv N = {v:Interval | N <= from v} @-}

-- Intuitively, an `LbItv n` is one that starts (at or) after `n`.
-- Next, we use the above to define an *ordered list*
-- of lower-bounded intervals:

{-@ type OrdItvs N = [LbItv N]<{\vHd vTl -> to vHd <= from vTl}> @-}

-- Legal Intervals
-- ---------------

{-@ data Intervals = Intervals { itvs :: OrdItvs 0 } @-}

-- LH will now ensure that illegal `Intervals` are not representable.

goodItvs  = Intervals [I 1 5, I 7 8, I 9 13]  -- LEGAL

badItvs1  = Intervals [I 1 7, I 5 8]          -- ILLEGAL: overlap!
badItvs2  = Intervals [I 1 5, I 9 13, I 7 8]  -- ILLEGAL: disorder!


-- Do the types _really_ capture the legality requirements?

{-@ goodLIs :: lb:Nat -> is:OrdItvs lb -> {v : Bool | v } @-}
goodLIs :: Int -> [Interval] -> Bool
goodLIs _ []              = True
goodLIs lb ((I f t) : is) = lb <= f && f < t && goodLIs t is


-- Algorithms on Intervals
-- -----------------------


intersect :: Intervals -> Intervals -> Intervals
intersect (Intervals is1) (Intervals is2) = Intervals (go 0 is1 is2)
  where
    {-@ go :: lb:Int -> OrdItvs lb -> OrdItvs lb -> OrdItvs lb @-}
    go _ _ [] = []
    go _ [] _ = []
    go lb (i1@(I f1 t1) : is1) (i2@(I f2 t2) : is2)
      -- reorder for symmetry
      | t1 < t2   = go lb (i2:is2) (i1:is1)
      -- disjoint
      | f1 >= t2  = go lb (i1:is1) is2
      -- subset
      | t1 == t2  = I f' t2 : go t2 is1 is2
      -- overlapping
      | f2 < f1   = (I f' t2 : go t2 (I t2 t1 : is1) is2)
      | otherwise = go lb (I f2 t1 : is1) (i2:is2)
      where f'    = max f1 f2


union :: Intervals -> Intervals -> Intervals
union (Intervals is1) (Intervals is2) = Intervals (go 0 is1 is2)
  where
    {-@ go :: lb:Int -> OrdItvs lb -> OrdItvs lb -> OrdItvs lb @-}
    go _ is [] = is
    go _ [] is = is
    go lb (i1@(I f1 t1) : is1) (i2@(I f2 t2) : is2)
      -- reorder for symmetry
      | t1 < t2 = go lb (i2:is2) (i1:is1)
      -- disjoint
      | f1 > t2 = i2 : go t2 (i1:is1) is2
      -- overlapping
      | otherwise  = go lb ( (I f' t1) : is1) is2
      where
        f' = min f1 f2


subtract :: Intervals -> Intervals -> Intervals
subtract (Intervals is1) (Intervals is2) = Intervals (go 0 is1 is2)
  where
    {-@ go :: lb:Int -> OrdItvs lb -> OrdItvs lb -> OrdItvs lb @-}
    go _ is [] = is
    go _ [] _  = []
    go lb (i1@(I f1 t1) : is1) (i2@(I f2 t2) : is2)
      -- i2 past i1
      | t1 <= f2  = (i1 : go t1 is1 (i2:is2))
      -- i1 past i2
      | t2 <= f1  = (go lb (i1:is1) is2)
      -- i1 contained in i2
      | f2 <= f1, t1 <= t2 = go lb is1 (i2:is2)
      -- i2 covers beginning of i1
      | f2 <= f1 = go t2 (I t2 t1 : is1) is2
      -- -- i2 covers end of i1
      | t1 <= t2 = ((I f1 f2) : go f2 is1 (i2:is2))
      -- i2 in the middle of i1
      | otherwise = (I f1 f2 : go f2 (I t2 t1 : is1) is2)

