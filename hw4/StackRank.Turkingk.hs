module StackTypes where


--
-- * Part 1: A Rank-Based Type System for the Stack Language
--
-- ** The abstract syntax
--
type Prog = [Cmd]

data Cmd = Push Int
         | Pop Int
         | Add
         | Mul
         | Dup
         | Inc
         | Swap
         deriving(Eq,Show)

type Stack = [Int]

type Rank = Int

-- NOTES:      (popsnum on stack,pushes on stack)
type CmdRank = (Int,Int)


-- ** The semantic function that yields the semantics of a program
--
prog :: Prog -> Stack -> Maybe Stack
prog []     s = Just s
prog (c:cs) s = cmd c s >>= prog cs


-- ** The semantics function that yields the semantics of a command
--
cmd :: Cmd -> Stack -> Maybe Stack
cmd (Push n) s       = Just (n:s)
cmd (Pop  k) s       = Just (drop k s)
cmd Add      (n:k:s) = Just (n + k:s)
cmd Mul      (n:k:s) = Just (n * k:s)
cmd Dup      (n:s)   = Just (n:n:s)
cmd Inc      (n:s)   = Just (n + 1:s)
cmd Swap     (n:k:s) = Just (k:n:s)
cmd _        _       = Nothing



-- | 1. Define the function rankC that maps each stack operation to its rank
--
rankC :: Cmd -> CmdRank
rankC (Push _) = (0,1)
rankC (Pop x)  = (x,0)
rankC (Add)    = (2,1)
rankC (Mul)    = (2,1)
rankC (Dup)    = (1,2)
rankC (Inc)    = (1,1)
rankC (Swap)   = (2,2)

-- | 2. Define the auxiliary function rankP that computes the rank of programs
--
rankP :: Prog -> Rank -> Maybe Rank
rankP [] x = Just x
rankP cmds x = rank cmds x

-- | helper function as specficied from the hints
rank :: Prog -> Rank -> Maybe Rank
rank [] x = Just x
rank (x:xs) r = let (popsnum,pushsnum) = rankC x in
                    if (popsnum < r) then rank xs (r - popsnum + pushsnum)
                    else Nothing

-- | 3. Define the semantic function semStatTC for evaluating stack programs
--
semStatTC :: Prog -> Stack -> Maybe Stack
semStatTC prg stk = if (rankP prg (length stk)) == Nothing then Nothing
                    else prog prg stk


-- | EXTRA CREDIT
--

-- This is a redefinition of the same data type
--  essentially it just compresses the definition together into one type
type PrgStk = Prog -> Stack -> Maybe Stack

prog' :: PrgStk
prog' []     s = Just s
prog' (c:cs) s = cmd c s >>= prog' cs



-- * Part 2: Runtime Stack
--
-- | Consider the following block of code.
--  1: { int x;
--  2:   int y;
--  3:   y := 1;
--  4:   { int f(int x) {
--  5:       if x = 0 then {
--  6:          y := 1 }
--  7:       else {
--  8:          y := f(x - 1) * y + 1 };
--  9:       return y;
-- 10:     }
-- 11:     x := f(2);
-- 12:   };
-- 13: }
--
-- | Illustrate the computations that take place during the evaluation of this block, that is,
--   draw a sequence of pictures each showing the complete runtime stack with all activation records after each statement or function call.

-- Assumptions:
-- Dynamic Scoping --
-- PEMDAS math notation --
--
-- Note: prime notation is for keeping track of scope
--
--[]                                            -- nothing on stack
--[(x:?)]                                       -- pushing X reference
--[(y:?),(x:?)]                                 -- pushing y reference
--[(y:1),(x:?)]                                 -- setting y = 1 from y = ?
--[f{},(y:1),(x:?)]                             -- pushing f function
--[(x':2),f{},(y:1),(x:?)]                      -- first call of f(2)
--[(x'':1),(x':2),f{},(y:1),(x:?)]              -- recursive call from f(2) into f(1)
--[(x''':0),(x'':1),(x':2),f{},(y:1),(x:?)]     -- recursive call from f(2) from f(1) into f(0)
--[(x''':0),(x'':1),(x':2),f{},(y:1),(x:?)]     -- recursive f(0) call exits  but sets y = 1 (from y = 1)
--[(x'':1),(x':2),f{},(y:2),(x:?)]              -- returns f(1) call exits but sets y = (f(0) * 1) + 1 = 2
--[(x':2),f{},(y:3),(x:?)]                      -- returns f(2) call exits but sets y = (f(1) * 1) + 1 = 3
--[f{},(y:3),(x:3)]                             -- sets x = (f(2) which returns y) = 3

-- * Part 3: Static and Dynamic Scope
--
-- | Consider the following block (assume call-by-value parameter passing).
--  1: { int x;
--  2:  int y;
--  3:  int z;
--  4:  x := 3;
--  5:  y := 7;
--  6:  { int f(int y) { return x * y };
--  7:    int y;
--  8:    y := 11;
--  9:    { int g(int x) { return f(y) };
-- 10:      { int y;
-- 11:        y := 13;
-- 12:        z := g(2);
-- 13:       };
-- 14:     };
-- 15:   };
-- 16: }
--  
-- | 1. Determine which value will be assigned to z in line 12 under static scoping.
-- | 2. Determine which value will be assigned to z in line 12 under dynamic scoping
-- 
-- Note: prime notation is for keeping track of scope
--
-- []                                                                     -- Nothing on stack
-- [(x:?)]                                                                -- push X reference
-- [(y:?),(x:?)]                                                          -- push Y reference
-- [(z:?),(y:?),(x:?)]                                                    -- push Z reference
-- [(z:?),(y:?),(x:3)]                                                    -- Setting x = 3 from x = ?
-- [(z:?),(y:7),(x:3)]                                                    -- Setting y = 7 from y = ? 
-- [f{},(z:?),(y:7),(x:3)]                                                -- Pushing f function
-- [(y':?),f{},(z:?),(y:7),(x:3)]                                         -- Pushing y' (new scope y) reference
-- [(y':11),f{},(z:?),(y:7),(x:3)]                                        -- Setting y' = 11 from y' = ?
-- [g{},(y':11),f{},(z:?),(y:7),(x:3)]                                    -- Pushing g function
-- [(y'':?),g{},(y':11),f{},(z:?),(y:7),(x:3)]                            -- Setting y'' (another new scope y) reference
-- [(y'':13),g{},(y':11),f{},(z:?),(y:7),(x:3)]                           -- Setting y'' = 13 from y'' = ?
-- [(x'g(2):2),(y'':13),g{},(y':11),f{},(z:?),(y:7),(x:3)]                -- Calling g(2), pushing parameter x from scope g(2)
-- [(y'f(13):13),(x'g(2):2),(y'':13),g{},(y':11),f{},(z:?),(y:7),(x:3)]   -- Calling f(13) pushing parameter y from scope f(13)
-- [(y'':13),g{},(y':11),f{},(z:?),(y:7),(x:3)]                           -- return from g(2){f(13){ return ...}} and set z = ? to z = ... depending on static or dynamic scoping

-- Notes: 
-- if using dyanmic scope disregard primes on varables 
--  dyanmic scoping is similiar to javascript/typescipt 'var' variable declaration
--  OR using global in C
-- if using static scope use out most varible that has been set.
--  Static scoping is similiar to regular declaration in c (non-global)     

-- static scoping  z = 3 * 7 = 21
-- dynamic scoping z = 2 * 13 = 26