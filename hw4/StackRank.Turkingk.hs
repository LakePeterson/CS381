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
rankC = undefined



-- | 2. Define the auxiliary function rankP that computes the rank of programs
--
rankP = undefined



-- | 3. Define the semantic function semStatTC for evaluating stack programs
--
semStatTC = undefined


-- | EXTRA CREDIT
--
prog' = undefined



-- * Part 2: Runtime Stack
--



-- * Part 3: Static and Dynamic Scope
--
