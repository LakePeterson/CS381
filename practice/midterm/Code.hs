data Code = A Code Int   -- A for "at"
          | P Code Code  -- P for "plus"
          | C Char       -- C for "char"
  deriving Show

-- 'b' @ 3
c1 = A (C 'b') 3

-- 3 + 'c' @ 4
c2 = undefined -- this one's an error

-- 'a' + ('b' @ 3)
c3 = P (C 'a') (A (C 'b') 3)

-- ('a' + 'b') @ 3
c4 = A (P (C 'a') (C 'b')) 3

-- 'a' @ 3 @ 4 @ 5
c5 = A (A (A (C 'a') 3) 4) 5