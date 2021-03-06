module MiniLogo where

import Data.List

--
-- * MiniLogo
--
-- | The grammar:
--      num ::= (any natural number)
--      var ::= (any variable name)
--    macro ::= (any macro name)
--
--     prog ::= ε | cmd; prog                 sequence of commands
--
--     mode ::= up | down                     pen status
--
--     expr ::= var                           variable reference
--           |  num                           literal number
--           |  expr + expr                   addition expression
--
--      cmd ::= pen mode                      change pen status
--           |  move (expr, expr)             move pen to a new position
--           |  define macro (var*) {prog}    define a macro
--           |  call macro (expr*)            invoke a macro

-- | 1. Define the abstract syntax as a set of Haskell data types.
--



-- | 2. Define a MiniLogo macro "line."
--
--      Concrete syntax in a comment:
--
--
--
--
--      Abstract syntax in code (include correct type header):
--
line = undefined


-- | 3. Define a MiniLogo macro "nix" using "line" defined above.
--
--      Concrete syntax in a comment:
--
--
--
--
--      Abstract syntax in code (include correct type header):
--
nix = undefined


-- | 4. Define a Haskell function "steps" (steps :: Int -> Prog) that draws
--      a staircase of n steps starting from (0,0).
--
steps = undefined


-- | 5. Define a Haskell function "macros" (macros :: Prog -> [Macro] that
--      returns a list of the names of all the macros that are defined anywhere
--      in a given MiniLogo program.
--
macros = undefined


-- | 6. Define a Haskell function "pretty" (pretty :: Prog -> String) that
--      "pretty-prints" a MiniLogo program.
--
pretty = undefined


--
-- * Bonus Problems
--
-- | 7. Define a Haskell function "optE" (optE :: Expr -> Expr) that partially
--      evaluates expressions by replacing additions of literals with the
--      result.
--
optE = undefined


-- | 8. Define a Haskell function "optP" (optP :: Prog -> Prog) that optimizes
--      all of the expressions contained in a given program using optE.
--
optP = undefined
