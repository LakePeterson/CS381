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

--data Mode == pen off page
            -- pen on page
data Mode = Up
          | Down
          deriving(Show,Eq)

--data Expr == Var String
            -- Num Int
            -- Add Expr Expr

data Expr = Var Var
          | Num Num
          | Add Expr Expr
          deriving (Eq,Show)

--data Cmd == up or down
           -- move x y
           -- define "name" [parameters]
           --       [Program instructions]
           -- Call "name" [parameters]
data Cmd = Pen Mode
         | Move Expr Expr
         | Define Macro [Var] Prog
         | Call Macro [Expr]
         deriving(Eq,Show)

-- Task 2 Line --
-- the x# and y# can theoretically be replaced by ints
line :: Cmd
line = Define "line" ["x1","y1","x2","y2"]
       [Pen Up,     Move (Var "x1") (Var "y1"),
        Pen Down,   Move (Var "x2") (Var "y2")]

-- Task 3 nix --
-- (x,y+h) \/ (x+w,y+h)
--   (x,y) /\ (x+w,y)
-- line1 == (x,y) to (x+w,y+h)
-- line2 == (x+w,y) to (x,y+h)
nix :: Cmd
nix = Define "nix" ["x","y","w","h"]
      [Call "line" [Var "x",Var "y",Add (Var "x") (Var "w"), Add (Var "y") (Var "h")],
       Call "line" [Add (Var "x") (Var "w"),Var "y",Var "x", Add (Var "y") (Var "h")]]

-- Task 4 steps --

-- Task 5 macros --

-- Task 6 pretty --

