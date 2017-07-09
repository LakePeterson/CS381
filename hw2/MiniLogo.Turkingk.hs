-------------------------------------------------
-- Kevin Turkington
-- 7/29/17
-- CS 381 Keeley Abbott
-- HW1
-------------------------------------------------

module MiniLogo where

import Prelude hiding (Num)
import Data.List

-- Task 1 Syntax --

type Num = Int
type Var = String
type Macro = String

type Prog = [Cmd]

data Mode = Up
          | Down
          deriving(Show,Eq)

--data Expr == Var String
            -- Num Int
            -- Add Expr Expr

data Expr = Ref Var
          | Lit Num
          | Add Expr Expr
          deriving (Eq,Show)


data Cmd = Pen Mode
         | Move Expr Expr
         | Define Macro [Var] Prog
         | Call Macro [Expr]
         deriving(Eq,Show)

-- Task 2 Line --
line :: Cmd
line = Define "line" ["x1","y1","x2","y2"]
       [Pen Up,     Move (Ref "x1") (Ref "y1"),
        Pen Down,   Move (Ref "x2") (Ref "y2")]
-- line = Define "line" [Lit x1,Lit y1,Lit x2,Lit y2]
--        [Pen Up,     Move (Lit x1) (Lit y1),
--         Pen Down,   Move (Lit x2) (Lit y2)]

-- Task 3 nix --

-- Task 4 steps --

-- Task 5 macros --

-- Task 6 pretty --

