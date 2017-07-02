-------------------------------------------------
-- Kevin Turkington
-- 7/29/17
-----------------------------------------------
module Nat where

import Prelude hiding (Enum(..), sum)


--
-- * Part 2: Natural numbers
--

-- | The natural numbers.
data Nat = Zero
         | Succ Nat
         deriving (Eq,Show)

-- | The number 1.
one :: Nat
one = Succ Zero

-- | The number 2.
two :: Nat
two = Succ one

-- | The number 3.
three :: Nat
three = Succ two

-- | The number 4.
four :: Nat
four = Succ three
-- | Nat = [Zero, Succ Zero, Succ one, Succ two, Succ three]

-- | The predecessor of a natural number.
--
--   >>> pred Zero
--   Zero
--
--   >>> pred three
--   Succ (Succ Zero)
--
-- | look for a better way of doing this
pred :: Nat -> Nat
pred Zero = Zero
pred (Succ x) = x

-- | True if the given value is zero.
--
--   >>> isZero Zero
--   True
--
--   >>> isZero two
--   False
--
-- | assuming i need to use the Nat dataType
isZero :: Nat -> Bool
isZero Zero = True
isZero _ = False


-- | Convert a natural number to an integer.
--
--   >>> toInt Zero
--   0
--
--   >>> toInt three
--   3
--
toInt :: Nat -> Int
toInt Zero = 0
toInt x = (toInt (pred x)) + 1


-- | Add two natural numbers.
--
--   >>> add one two
--   Succ (Succ (Succ Zero))
--
--   >>> add Zero one == one
--   True
--
--   >>> add two two == four
--   True
--
--   >>> add two three == add three two
--   True
--
add :: Nat -> Nat -> Nat
add Zero x = x
add x Zero = x
add x y = add (pred x) (Succ y)
--add x y = [Zero,one,two,three,four] !! ((toInt x) + (toInt y))

-- | Subtract the second natural number from the first. Return zero
--   if the second number is bigger.
--
--   >>> sub two one
--   Succ Zero
--
--   >>> sub three one
--   Succ (Succ Zero)
--
--   >>> sub one one
--   Zero
--
--   >>> sub one three
--   Zero
--
sub :: Nat -> Nat -> Nat
sub Zero x = Zero
sub x Zero = x
sub x y = sub (pred x) (pred y)


-- | Is the left value greater than the right?
--
--   >>> gt one two
--   False
--
--   >>> gt two one
--   True
--
--   >>> gt two two
--   False
--
gt :: Nat -> Nat -> Bool
gt x y = (toInt x) > (toInt y)


-- | Multiply two natural numbers.
--
--   >>> mult two Zero
--   Zero
--
--   >>> mult Zero three
--   Zero
--
--   >>> toInt (mult two three)
--   6
--
--   >>> toInt (mult three three)
--   9
--
mult :: Nat -> Nat -> Nat
mult x Zero = Zero
mult Zero x = Zero
--mult x y = nums !! ((toInt x) * (toInt y))
mult x y = sum(take (toInt x) (repeat y))


-- | Compute the sum of a list of natural numbers.
--
--   >>> sum []
--   Zero
--
--   >>> sum [one,Zero,two]
--   Succ (Succ (Succ Zero))
--
--   >>> toInt (sum [one,two,three])
--   6
--
sum :: [Nat] -> Nat
sum [] = Zero
sum (h:t) = foldr (add) Zero (h:t)

-- | An infinite list of all of the *odd* natural numbers, in order.
--
--   >>> map toInt (take 5 odds)
--   [1,3,5,7,9]
--
--   >>> toInt (sum (take 100 odds))
--   10000
--
odds :: [Nat]
odds = filter isOdd nums

isOdd :: Nat -> Bool
isOdd Zero = False
isOdd x = not ((mod (toInt x) 2) == 0)

nums :: [Nat]
nums = Zero : map (add one) nums
