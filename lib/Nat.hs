{-# LANGUAGE NPlusKPatterns #-}

-- |
-- Module      : Nat
-- Description : Natural numbers as a datatype with base functor Either () a
-- Copyright   : (c) 2026 Eduardo Freitas Fernandes
-- License     : MIT
-- Maintainer  : eduardof.fernandes05@gmail.com
-- Stability   : experimental
--
-- This module provides combinators for working with the natural numbers,
-- viewed through their base functor 'Either () a'. The zero case corresponds
-- to the left injection and the successor case to the right injection,
-- enabling a point-free, recursion-scheme style of programming.
module Nat where

import Algebra
  ( add,
    i1,
    i2,
    mul,
    one,
    p1,
    p2,
    zero,
    (-|-),
    (/\),
    (\/),
  )

-- Datatype definition -------------------------------------------------------------

-- The natural numbers are represented by Haskell's 'Int' / 'Integral' types.
-- The base functor is 'Either () a', where 'Left ()' represents zero
-- and 'Right n' represents the predecessor of a successor.

-- | The constructor of the base functor for natural numbers,
-- mapping 'Either () a' to 'Int' by sending zero to @0@ and
-- a predecessor @n@ to @succ n@.
inNat :: Either a Int -> Int
inNat = zero \/ succ

-- | The destructor of the base functor for natural numbers,
-- mapping an 'Integral' value to 'Either () a' by sending @0@ to 'Left ()'
-- and a successor @n+1@ to 'Right n'.
outNat :: (Integral a) => a -> Either () a
outNat 0 = i1 ()
outNat (n + 1) = i2 n
outNat _ = error "Invalid input"

-- | The base functor algebra for natural numbers over the shape 'Either () a'.
baseNat :: p -> (a -> b) -> Either () a -> Either () b
baseNat _ f = id -|- f

-- Ana + Cata + Hylo ---------------------------------------------------------------

-- | The recursive step for the natural number shape, expressed as a map
-- over the base functor 'Either () a'.
recNat :: (a -> b) -> Either () a -> Either () b
recNat f = baseNat id f

-- | The catamorphism for natural numbers, folding an 'Integral' value
-- through its base functor by applying an algebra @g :: Either () b -> b@.
cataNat :: (Integral a) => (Either () b -> b) -> a -> b
cataNat g = g . recNat (cataNat g) . outNat

-- | The anamorphism for natural numbers, unfolding a seed value
-- into an 'Int' via a coalgebra @g :: a -> Either () a@.
anaNat :: (a -> Either () a) -> a -> Int
anaNat g = inNat . recNat (anaNat g) . g

-- | The hylomorphism for natural numbers, combining an anamorphism
-- and a catamorphism into a single pass.
hyloNat :: (Either () a -> a) -> (b -> Either () b) -> b -> a
hyloNat f g = cataNat f . anaNat g

-- Others --------------------------------------------------------------------------

-- | Applies a function @b@ exactly @n@ times to an initial value @i@.
-- Equivalent to iterating a function a fixed number of times.
for :: (Integral a) => (b -> b) -> b -> a -> b
for b i = cataNat (const i \/ b)

-- | Adds @a@ to a natural number @n@ by iterating 'succ' @n@ times.
addition :: (Integral a, Enum b) => b -> a -> b
addition a = cataNat (const a \/ succ)

-- | Multiplies @a@ by a natural number @n@ by iterating addition @n@ times.
multiplication :: (Integral a) => Int -> a -> Int
multiplication a = cataNat (zero \/ (a +))

-- | Raises @a@ to the power of a natural number @n@ by iterating multiplication @n@ times.
exp :: (Integral a) => Int -> a -> Int
exp a = cataNat (one \/ (a *))

-- | Computes the square of a natural number using a catamorphism that
-- simultaneously tracks the accumulated square and the current step value.
squareOf :: Integer -> Int
squareOf = p1 . cataNat ((zero \/ add) /\ (one \/ ((2 +) . p2)))

-- | Computes the factorial of a natural number using a catamorphism that
-- simultaneously tracks the accumulated product and the current counter.
facNat :: Integer -> Int
facNat = p1 . cataNat ((one \/ mul) /\ (one \/ (succ . p2)))

-- | Integer division. Divides the first argument by the second by counting
-- how many times the divisor fits into the dividend.
idiv :: Integer -> Integer -> Int
idiv = flip aux
  where
    aux x = anaNat (divide x)
    divide b a
      | a < b = i1 ()
      | otherwise = i2 (a - b)

-- | Bubble sort. Sorts a list by repeatedly applying a single bubble pass
-- @n@ times, where @n@ is the length of the list.
bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort xs = for bubble xs (length xs)
  where
    bubble (h : y : t)
      | h > y = y : bubble (h : t)
      | otherwise = h : bubble (y : t)
    bubble x = x

-- | Computes the @n@-th Fibonacci number using a catamorphism that
-- simultaneously tracks two consecutive Fibonacci values.
fibNat :: Integer -> Int
fibNat = p2 . cataNat ((one \/ add) /\ (one \/ p1))

-- | Replicates the application of a function @f@ exactly @n@ times,
-- returning the composition of @f@ with itself @n@ times.
replicate :: (Integral a1) => (a2 -> a2) -> a1 -> a2 -> a2
replicate f = cataNat (const id \/ (f .))
