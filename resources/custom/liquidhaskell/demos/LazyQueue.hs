
module LazyQueue (Queue, insert, remove, emp) where

-- Source: Okasaki, JFP 1995
-- http://www.westpoint.edu/eecs/SiteAssets/SitePages/Faculty%20Publication%20Documents/Okasaki/jfp95queue.pdf


{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--total"          @-}
{-@ LIQUID "--maxparams=3"    @-}

import Prelude hiding (length)

-- | Size function actually returns the size: (Duh!)

{-@ size :: q:SList a -> {v:Nat | v = size q} @-}
data Queue a = Q  { front :: SList a
                  , back  :: SList a
                  }

--------------------------------------------------------------------
-- | Sized Lists
--------------------------------------------------------------------

data SList a = SL { size :: Int, elems :: [a]}

-- | But how can we be sure that `size` is indeed the *real size* of `elems`?

-- Lets write a function to *measure* the real size:

{-@ measure realSize @-}
realSize      :: [a] -> Int
realSize []     = 0
realSize (_:xs) = 1 + realSize xs


-- | specify a *refined* type for `SList` that ensures the *real* size is saved in the `size` field:

{-@ data SList a = SL {
       size  :: Nat
     , elems :: {v:[a] | realSize v = size}
     }
  @-}


-- As a sanity check, consider this:

okList  = SL 1 ["cat"]    -- accepted

badList = SL 1 []         -- rejected



-- | SList of size N

{-@ type SListN a N = {v:SList a | size v = N} @-}

-- | Non-Empty SLists:

{-@ type NEList a = {v:SList a | size v > 0} @-}



-- | To Construct lists, we use `nil` and `cons`:


{-@ nil          :: SListN a 0  @-}
nil              = SL 0 []

{-@ cons         :: a -> xs:SList a -> SListN a {size xs + 1}   @-}
cons x (SL n xs) = SL (n+1) (x:xs)


-- | To Destruct lists, we have `hd` and `tl`

{-@ tl           :: xs:NEList a -> SListN a {size xs - 1}  @-}
tl (SL n (_:xs)) = SL (n-1) xs

{-@ hd           :: xs:NEList a -> a @-}
hd (SL _ (x:_))  = x


-- Don't worry, they are perfectly *safe* as LiquidHaskell will make
-- sure we *only* call these operators on non-empty `SList`s. For example,

okHd  = hd okList       -- accepted

badHd = hd (tl okList)  -- rejected


--------------------------------------------------------------------------
-- | Queue Type
--------------------------------------------------------------------------

{-@ data Queue a = Q {
       front :: SList a
     , back  :: SListLE a (size front)
     }
  @-}

-- | Lists with less than `N` elements:

{-@ type SListLE a N = {v:SList a | size v <= N} @-}


-- As a quick check, notice that we *cannot represent illegal Queues*:

okQ  = Q okList nil  -- accepted, |front| > |back|

badQ = Q nil okList  -- rejected, |front| < |back|


-- | To Measure Queue Size

{-@ measure qsize @-}
qsize         :: Queue a -> Int
qsize (Q l r) = size l + size r


-- | This will prove helpful to define `Queue`s of a given size `N` and
--   non-empty `Queue`s (from which values can be safely removed.)

{-@ type QueueN a N = {v:Queue a | N = qsize v} @-}
{-@ type NEQueue a  = {v:Queue a | 0 < qsize v} @-}


--------------------------------------------------------------------------
-- | Queue Type
--------------------------------------------------------------------------


-- | The Empty Queue is simply one where both `front` and `back` are `nil`.

{-@ emp :: QueueN a 0 @-}
emp = Q nil nil


-- | To Insert an element we just `cons` it to the `back` list, and call
-- the *smart constructor* `makeq` to ensure that the balance invariant holds:

{-@ insert       :: a -> q:Queue a -> QueueN a {qsize q + 1}   @-}
insert e (Q f b) = makeq f (e `cons` b)


-- | To Remove an element we pop it off the `front` by using `hd` and `tl`.
-- Notice that the `remove` is only called on non-empty `Queue`s, which together
-- with the key balance invariant, ensures that the calls to `hd` and `tl` are safe.

{-@ remove       :: q:NEQueue a -> (a, QueueN a {qsize q - 1}) @-}
remove (Q f b)   = (hd f, makeq (tl f) b)


-- | To Ensure the Invariant, we use the smart constructor `makeq`.

{-@ makeq :: f:SList a
          -> b:SListLE a {size f + 1 }
          -> QueueN a {size f + size b}
  @-}
makeq l r
  | size r <= size l = Q l r
  | otherwise        = Q (rot l r nil) nil


-- | The Rotate function is only called when the `back` is one larger
-- than the `front` (we never let things drift beyond that). 

{-@ rot :: l:SList a
        -> r:SListN _ {1 + size l}
        -> a:SList _
        -> SListN _ {size l + size r + size a}
  @-}
rot l r a
  | size l == 0 = (hd r) `cons` a
  | otherwise   = (hd l) `cons` (rot (tl l) (tl r) ((hd r) `cons` a))






