-- | Illustration of static vs. dynamic scope.
module Scope where

import Trace
import Data.Maybe


-- | Variable names.
type Var = String

-- | Abstract syntax.
data Exp = Lit Int          -- integer literal
         | Add Exp Exp      -- addition expression
         | Let Var Exp Exp  -- variable binding
         | Ref Var          -- variable reference
         | Fun Var Exp      -- anonymous function w/ one argument
         | App Exp Exp      -- function application
  deriving (Eq,Show)

-- Example programs ...

-- \x -> x+1
suc :: Exp
suc = Fun "x" (Add (Ref "x") (Lit 1))


-- let succ = \x -> x+1 (function declaration)
declSucc :: Exp -> Exp
declSucc = Let "succ" suc


-- in succ (succ 5)
callSucc :: Exp
callSucc = declSucc
  $ App (Ref "succ") (App (Ref "succ") (Lit 5))


-- | An example that illustrates the difference between
--   static and dynamic scope. Is the result 12 or 13?
--
-- let z = 2 in
--   let f = (\x -> x+z) in
--     let z = 3 in
--       f 10
ex :: Exp
ex = Let "z" (Lit 2)
  $ Let "f" (Fun "x" (Add (Ref "x") (Ref "z")))
  $ Let "z" (Lit 3)
  $ (App (Ref "f") (Lit 10))



-- | Define call-by-value semantics
--

-- | An environment maps variables to some type of values.
type Env v = [(Var,v)]


-- ** Dynamic scoping, call-by-value

-- | A value is either an integer, a function, or an error.
data DVal = DI Int        -- integer value
          | DF Var Exp    -- function
          deriving Show


-- | Semantic function.
-- semantic domain: Env DVal -> Maybe DVal
dsem :: Exp -> Env DVal -> Maybe DVal
dsem (Lit n)       _ = Just (DI n)
dsem (Add e1 e2)   m = case (dsem e1 m,dsem e2 m) of
  (Just (DI n),Just (DI k)) -> Just (DI (n + k))
  _                         -> Nothing
dsem (Let v e1 e2) m = case dsem e1 m of
  Just n -> dsem e2 ((v,n):m)
  _      -> Nothing
dsem (Ref v)       m = lookup v m
dsem (Fun v e)     _ = Just (DF v e)
dsem (App e1 e2)   m = case (dsem e1 m,dsem e2 m) of
  (Just (DF v e),Just n) -> dsem e ((v,n):m)
  _                      -> Nothing


-- | Stack trace evaluation
--
isFun :: DVal -> Bool
isFun (DF _ _) = True
isFun _        = False

dsemT :: Exp -> Env DVal -> Trace Exp DVal DVal
dsemT a@(Lit i)     m = Tr a m [] (DI i)
dsemT a@(Add l r)   m = Tr a m [tl, tr] (addD (getVal tl) (getVal tr))
  where
    (tl, tr) = (dsemT l m, dsemT r m)
dsemT a@(Let x l r) m = Tr a m [tl, tr] (getVal tr)
  where
    tl = dsemT l m
    tr = dsemT r ((x,getVal tl):m)
dsemT a@(Ref x)     m = Tr a m [] (fromJust (lookup x m))
dsemT a@(Fun x e)   m = Tr a m [] (DF x e)
dsemT a@(App f e)   m
  | isFun tfv = Tr a m [tl,tfe] (getVal tfe)
  where
    tf     = dsemT f m
    tfv    = getVal tf
    DF x b = tfv
    tl     = dsemT e m
    tfe    = dsemT b ((x,getVal tl):m)


addD :: DVal -> DVal -> DVal
addD (DI i) (DI j) = (DI (i + j))



-- ** Static scoping, call-by-value

-- | A value is either an integer, a closure, or an error.
data SVal = SI Int                   -- integer value
          | SC (Env SVal) Var Exp    -- closure
          deriving Show


-- | Semantic function.
-- semantic domain: Env SVal -> Maybe SVal
ssem :: Exp -> Env SVal -> Maybe SVal
ssem (Lit n)       _ = Just (SI n)
ssem (Add e1 e2)   m = case (ssem e1 m,ssem e2 m) of
  (Just (SI n),Just (SI k)) -> Just (SI (n + k))
  _                         -> Nothing
ssem (Let v e1 e2) m = case ssem e1 m of
  Just n -> ssem e2 ((v,n):m)
  _      -> Nothing
ssem (Ref v)       m = lookup v m
ssem (Fun v e)     m = Just (SC m v e)
ssem (App e1 e2)   m = case (ssem e1 m,ssem e2 m) of
  (Just (SC m' v e),Just n) -> ssem e ((v,n):m')
  _                         -> Nothing


-- | Stack trace evaluation.
--
isClos :: SVal -> Bool
isClos (SC _ _ _) = True
isClos _          = False

ssemT :: Exp -> Env SVal -> Trace Exp SVal SVal
ssemT a@(Lit i)     m = Tr a m [] (SI i)
ssemT a@(Add l r)   m = Tr a m [tl, tr] (addS (getVal tl) (getVal tr))
  where
    (tl, tr) = (ssemT l m, ssemT r m)
ssemT a@(Let x l r) m = Tr a m [tl, tr] (getVal tr)
  where
    tl = ssemT l m
    tr = ssemT r ((x, getVal tl):m)
ssemT a@(Ref x)     m = Tr a m [] (fromJust (lookup x m))
ssemT a@(Fun x e)   m = Tr a m [] (SC m x e)
ssemT a@(App f e)   m
  | isClos tfv = Tr a m [tl, tfe] (getVal tfe)
  where
    tf        = ssemT f m
    tfv       = getVal tf
    SC m' x b = tfv
    tl        = ssemT e m
    tfe       = ssemT b ((x, getVal tl):m')


addS :: SVal -> SVal -> SVal
addS (SI i) (SI j) = (SI (i + j))


-- ** Naive call-by-name

-- | An expression that loops forever if evaluated.
loop :: Exp
loop = App (Fun "x" (App (Ref "x") (Ref "x")))
  (Fun "x" (App (Ref "x") (Ref "x")))

-- | Return 3 no matter the argument.
ret3 :: Exp
ret3 = Fun "z" (Lit 3)


-- | An example where call-by-value loops and call-by-name terminates.
ex2 :: Exp
ex2 = App ret3 loop


-- | Semantic function.
nsem :: Exp -> Env Exp -> Maybe DVal
nsem (Lit n)       _ = Just (DI n)
nsem (Add e1 e2)   m = case (nsem e1 m, nsem e2 m) of
  (Just (DI n), Just (DI k)) -> Just (DI (n + k))
  _                          -> Nothing
nsem (Let v e1 e2) m = nsem e2 ((v,e1):m)
nsem (Ref v)       m = case lookup v m of
  Just e -> nsem e m
  _      -> Nothing
nsem (Fun v e)     m = Just (DF v e)
nsem (App e1 e2)   m = case nsem e1 m of
  Just (DF v e) -> nsem e ((v,e2):m)
  _             -> Nothing



-- ** Naive call-by-need (lazy evaluation)

-- | A function that adds 0 to y a million times.
slow :: Exp
slow = Fun "y" $ foldr ($) (Ref "y") $ replicate 1000000 (Add (Lit 0))

ex3 :: Exp
ex3 = Let "quadruple" (Fun "x" (Add (Add (Ref "x") (Ref "x"))
                                    (Add (Ref "x") (Ref "x"))))
  $ App (Ref "quadruple") (App slow (Lit 42))


-- | Environment for lazy evaluation.
type LEnv = Env (Either Exp DVal)

-- | Semantic function.
lsem :: Exp -> LEnv -> Maybe (LEnv, DVal)
lsem (Lit n)       m = Just (m, DI n)
lsem (Add e1 e2)   m = case lsem e1 m of
  Just (m1, DI n) -> case lsem e2 m1 of
    Just (m2, DI k) -> Just (m2, DI (n + k))
    _               -> Nothing
  _               -> Nothing
lsem (Let v e1 e2) m = lsem e2 ((v, Left e1):m)
lsem (Ref v)       m = case lookup v m of
  Just (Right x) -> Just (m, x)
  Just (Left e)  -> case lsem e m of
    Just (m', x') -> Just ((v, Right x'):m', x')
    _             -> Nothing
  _              -> Nothing
lsem (Fun v e)     m = Just (m, DF v e)
lsem (App e1 e2)   m = case lsem e1 m of
  Just (m1, DF v e) -> lsem e ((v, Left e2):m1)
  _                 -> Nothing


-- | Evaluate with call-by-name evaluation and an empty environment.
name :: Exp -> Maybe DVal
name e = nsem e []


-- | Evaluate with lazy evaluation and an empty environment.
lazy :: Exp -> Maybe DVal
lazy e = fmap snd (lsem e [])


-- | Smart constructors to simplify the construction of syntax trees.
--
[i0,i1,i2,i3] = map Lit [0..3]
[x,y,z,f] = map Ref ["x","y","z","f"]


-- | Example expressions for stack tracing.
--

-- functions
dbl = Fun "x" (Add x x)
-- applications
ds3 = App dbl (App suc i3)
-- definitions
letxy = Let "x" i1 (Let "y" i2 (Add x y))
letxy' = Let "x" i1 (Add (Let "y" i2 y) x)
letxx = Let "x" i1 (Add (Let "x" i2 x) x)
noRec = Let "x" x x
noRec' = Let "x" x i1
-- function definitions
letfx = Let "x" (Let "f" suc (App f i1)) x
-- call-by-value vs call-by-name evaluation
one = Let "x" y i1
four = Let "f" suc (App f (Add i1 i2))
-- dynamic scope
-- f = Fun "x" (Add x (Fun y ()))
d1 = Let "x" i1
  (Let "f" (Fun "y" (y `Add` x))
   (Let "x" i2
    (App f i0)))
d2 = Let "z" i1 d1
d3 = Let "x" i1
  (Let "g" (Fun "x" (x `Add` x))
   (Let "x" i3 d2))

dtr :: Exp -> Trace Exp DVal DVal
dtr e = dsemT e []

str :: Exp -> Trace Exp SVal SVal
str e = ssemT e []
