{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--total"          @-}

module ANF (Op (..), Expr (..), isImm, isAnf, toAnf) where

import Control.Monad.State.Lazy

mkLet :: [(Var, AnfExpr)] -> AnfExpr -> AnfExpr
imm, immExpr :: Expr -> AnfM ([(Var, AnfExpr)], ImmExpr)
anf   :: Expr -> AnfM AnfExpr

type AnfExpr = Expr
type ImmExpr = Expr


-- | Source Language


data Expr
  = EInt  Int               -- ^ Integers
  | EVar  Var               -- ^ Variables
  | EBin  Op   Expr Expr    -- ^ Binary Operators
  | ELet  Var  Expr Expr    -- ^ Let-binders
  | ELam  Var  Expr         -- ^ Function definitions
  | EApp  Expr Expr         -- ^ Function applications
  deriving (Show)


type Var = String

data Op  = Plus | Minus | Times
         deriving (Show)



-- | srcExpr is ((2 + 3) * (12 - 4)) * (7 + 8)

srcExpr :: Expr
srcExpr = EBin Times
            (EBin Times
              (EBin Plus  (EInt  2) (EInt 3))
              (EBin Minus (EInt 12) (EInt 4)))
            (EBin Plus (EInt 7) (EInt 8))



-- | Immediate Expressions 

-- An `Expr` is **immediate** if it is a `Number` or a `Var`;
-- we can formalize this as a Haskell predicate:

{-@ measure isImm @-}
isImm :: Expr -> Bool
isImm (EInt _) = True
isImm (EVar _) = True
isImm _        = False

{-@ type ImmExpr = {v:Expr | isImm v} @-}


-- For example, `e1` is immediate but `e2` is not:

{-@ e1 :: ImmExpr @-}
e1 = EInt 7

{-@ e2 :: ImmExpr @-}
e2 = EBin Plus e1 e1


-- | ANF Expressions
--
-- Similiarly, an `Expr` is in **ANF** if all arguments for operators
-- and applications are **immediate**. Once again, we can formalize
-- this intuition as a Haskell predicate:


{-@ measure isAnf @-}
isAnf :: Expr -> Bool
isAnf (EInt {})      = True
isAnf (EVar {})      = True
isAnf (EBin _ e1 e2) = isImm e1 && isImm e2  -- args for operators
isAnf (EApp e1 e2)   = isImm e1 && isImm e2  -- must be immediate,
isAnf (ELet _ e1 e2) = isAnf e1 && isAnf e2  -- and sub-expressions
isAnf (ELam _ e)     = isAnf e               -- must be in ANF

-- and then use the predicate to define the subset of _legal_ ANF expressions:

{-@ type AnfExpr = {v:Expr | isAnf v} @-}

-- For example, `e2` above _is_ in ANF but `e3` is not:


{-@ e2' :: AnfExpr @-}
e2' = EBin Plus e1 e1

{-@ e3 :: AnfExpr @-}
e3 = EBin Plus e2' e2'

-- | As we need to generate "temporary" intermediate
-- binders, it will be convenient to work within a
-- monad that generates `fresh` variables:


type AnfM a = State Int a

fresh :: AnfM Var
fresh = do
  n <- get
  put (n+1)
  return ("anf" ++ show n)

-- | ANF Conversion

--------------------------------------------------------------------------------
{-@ anf :: Expr -> AnfM AnfExpr @-}
--------------------------------------------------------------------------------
anf (EInt n) =
  return (EInt n)

anf (EVar x) =
  return (EVar x)

anf (ELet x e1 e2) = do
  a1 <- anf e1
  a2 <- anf e2
  return (ELet x a1 a2)

anf (EBin o e1 e2) = do
  (b1s, v1) <- imm e1
  (b2s, v2) <- imm e2
  return (mkLet (b1s ++ b2s) (EBin o v1 v2))

anf (ELam x e) = do
  a <- anf e
  return (ELam x a)

anf (EApp e1 e2) = do
  (b1s, v1) <- imm e1
  (b2s, v2) <- imm e2
  return (mkLet (b1s ++ b2s) (EApp v1 v2))


-- | In `anf` the real work happens inside `imm` which takes an arbitary
-- _argument_ expression and makes it **immediate** by generating temporary
-- (ANF) bindings. The resulting bindings (and immediate values) are
-- composed by the helper `mkLet` that takes a list of binders and a body
-- `AnfExpr` and stitches them into a single `AnfExpr`:


{-@ mkLet :: [(Var, AnfExpr)] -> AnfExpr -> AnfExpr @-}
mkLet []         e' = e'
mkLet ((x,e):bs) e' = ELet x e (mkLet bs e')


-- | Making Arguments Immediate 

--------------------------------------------------------------------------------
{-@ imm :: Expr -> AnfM ([(Var, AnfExpr)], ImmExpr) @-}
--------------------------------------------------------------------------------
imm (EInt n)       = return ([], EInt n)
imm (EVar x)       = return ([], EVar x)
imm e@(ELet {})    = immExpr e
imm e@(ELam {})    = immExpr e
imm (EBin o e1 e2) = imm2 e1 e2 (EBin o)
imm (EApp e1 e2)   = imm2 e1 e2 EApp


{-@ immExpr :: Expr -> AnfM ([(Var, AnfExpr)], ImmExpr) @-}
immExpr e = do
  a <- anf e
  t <- fresh
  return ([(t, a)], EVar t)


-- | Finally, binary operators and applications are converted by `imm2` that
--   takes two arbitrary expressions and an expression constructor, yielding
--   the anf-binders and immediate expression.

imm2 :: Expr -> Expr -> (ImmExpr -> ImmExpr -> AnfExpr)
     -> AnfM ([(Var, AnfExpr)], ImmExpr)
imm2 e1 e2 f = do
  (b1s, v1) <- imm e1
  (b2s, v2) <- imm e2
  t         <- fresh
  let bs'    = b1s ++ b2s ++ [(t, f v1 v2)]
  return      (bs', EVar t)


toAnf :: Expr -> AnfExpr
toAnf e = evalState (anf e) 0


-- ghci> toAnf srcExpr
-- ELet "anf0" (EBin Plus (EInt 2) (EInt 3))
--  (ELet "anf1" (EBin Minus (EInt 12) (EInt 4))
--    (ELet "anf2" (EBin Times (EVar "anf0") (EVar "anf1"))
--      (ELet "anf3" (EBin Plus (EInt 7) (EInt 8))
--        (EBin Times (EVar "anf2") (EVar "anf3")))))

