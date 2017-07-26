module Exp where

data Exp = LitI Int
         | LitB Bool
         | Add  Exp Exp
         | If   Exp Exp Exp
         deriving(Eq,Show)

x :: Exp
x = If (LitB True) (Add (LitI 3) (LitI 3)) (LitI 6)

data Val = I Int | B Bool
         deriving(Eq,Show)

sem :: Exp -> Maybe Val
sem (LitI n) = Just (I n)
sem (LitB b) = Just (B b)
sem (Add l r) = case (sem l, sem r) of
  (Just (I n), Just (I k)) -> Just (I (n + k))
  _                        -> Nothing
sem (If c t e) = case sem c of
  (Just (B True))  -> sem t
  (Just (B False)) -> sem e
  _                -> Nothing

data Type = TInt | TBool
          deriving(Eq, Show)

typeOf :: Exp -> Maybe Type
typeOf (LitI _)   = Just TInt
typeOf (LitB _)   = Just TBool
typeOf (Add l r)  = case (typeOf l, typeOf r) of
  (Just TInt, Just TInt) -> Just TInt
  _                      -> Nothing
typeOf (If c t e) = case typeOf c of
  (Just TBool) -> if (typeOf t == typeOf e) then typeOf t else Nothing
  _            -> Nothing
