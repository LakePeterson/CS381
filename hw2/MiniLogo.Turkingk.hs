-------------------------------------------------
-- Kevin Turkington
-- 7/29/17
-- CS 381 Keeley Abbott
-- HW1
-------------------------------------------------

module MiniLogo where

import Prelude hiding (Num)
import Data.List

type Num = Int
type Var = String
type Macro = String

data Expr = Ref Var
          | Lit Num
          | Add Expr Expr
          deriving (Eq,Show)

data Mode = Up
          | Down
          deriving(Show,Eq)

