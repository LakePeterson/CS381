module MiniMiniLogoSem where

import MiniMiniLogo
import Render

--NOTES -----------------------------------------------
-- -- | A program is a sequence of commands.
-- type Prog = [Cmd]

-- -- | The mode of the pen.
-- data Mode = Down | Up
--   deriving (Eq,Show)

-- -- | Abstract syntax of commands.
-- data Cmd = Pen Mode
--          | Move Int Int
--   deriving (Eq,Show)

-- -- | A point is a cartesian pair (x,y).
-- type Point = (Int,Int)

-- -- | A line is defined by its endpoints.
-- type Line = (Point,Point)
-------------------------------------------------------



--
-- * Semantics of MiniMiniLogo
--

-- NOTE:
--   * MiniMiniLogo.hs defines the abstract syntax of MiniMiniLogo and some
--     functions for generating MiniMiniLogo programs. It contains the type
--     definitions for Mode, Cmd, and Prog.
--   * Render.hs contains code for rendering the output of a MiniMiniLogo
--     program in HTML5. It contains the types definitions for Point and Line.

-- | A type to represent the current state of the pen.
type State = (Mode,Point)

-- | The initial state of the pen.
start :: State
start = (Up,(0,0))

-- | A function that renders the image to HTML. Only works after you have
--   implemented `prog`. Applying `draw` to a MiniMiniLogo program will
--   produce an HTML file named MiniMiniLogo.html, which you can load in
--   your browswer to view the rendered image.
draw :: Prog -> IO ()
draw p = let (_,ls) = prog p start in toHTML ls


-- Semantic domains:
--   * Cmd:  State -> (State, Maybe Line)
--   * Prog: State -> (State, [Line])


-- | Semantic function for Cmd.
--
--   >>> cmd (Pen Down) (Up,(2,3))
--   ((Down,(2,3)),Nothing)
--
--   >>> cmd (Pen Up) (Down,(2,3))
--   ((Up,(2,3)),Nothing)
--
--   >>> cmd (Move 4 5) (Up,(2,3))
--   ((Up,(4,5)),Nothing)
--
--   >>> cmd (Move 4 5) (Down,(2,3))
--   ((Down,(4,5)),Just ((2,3),(4,5)))
--
cmd :: Cmd -> State -> (State, Maybe Line)
cmd (Pen x) (_,pt)      = (  (x,pt) , Nothing) 
cmd (Move m1 m2) (Down,pt) = (  (Down, (m1,m2)) , Just((pt),(m1,m2)) )
cmd (Move m1 m2) (Up,pt) = (  (Up, (m1,m2)) , Nothing)


-- | Semantic function for Prog.
--
--   >>> prog (nix 10 10 5 7) start
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
--   >>> prog (steps 2 0 0) start
--   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])
prog :: Prog -> State -> (State, [Line])
prog [] (x) = (x,[])
prog prgs x = (progStateBuilder prgs, --do things here )

progLinePt :: Cmd -> Maybe Point
progLinePt (Pen _)    = Nothing
progLinePt (Move x y) = Just((x,y))

progStateBuilder :: Prog -> State
progStateBuilder [] = start
progStateBuilder x  = (Down, (case progLinePt(last x) of
                                  Nothing -> (0,0)
                                  (Just pt) -> pt)
                      )


--
-- * Extra credit
--

-- | This should be a MiniMiniLogo program that draws an amazing picture.
--   Add as many helper functions as you want.
amazing :: Prog
amazing = undefined
