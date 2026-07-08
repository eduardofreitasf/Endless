{-# LANGUAGE NPlusKPatterns #-}

-- |
-- Module      : List
-- Description : Lists as a datatype with base functor Either () (a, [a])
-- Copyright   : (c) 2026 Eduardo Freitas Fernandes
-- License     : MIT
-- Maintainer  : eduardof.fernandes05@gmail.com
-- Stability   : experimental
--
-- This module provides combinators for working with the standard list type,
-- viewed through its base functor 'Either () (a, [a])'. The empty case
-- corresponds to the left injection and the cons case to the right injection,
-- enabling a point-free, recursion-scheme style of programming.
module List where

import Algebra
import Nat (outNat)

-- Datatype definition -------------------------------------------------------------

-- The List datatype is already defined in Haskell
-- data List a = Nil | Cons (a, List a)

-- | The constructor of the base functor for lists,
-- mapping 'Either () (a, [a])' to '[a]' by sending the empty case to @[]@
-- and a head-tail pair to a cons cell.
inList :: Either () (a, [a]) -> [a]
inList = nil \/ cons

-- | The destructor of the base functor for lists,
-- mapping '[a]' to 'Either () (a, [a])' by sending @[]@ to 'Left ()'
-- and @(h:t)@ to 'Right (h, t)'.
outList :: [a] -> Either () (a, [a])
outList [] = i1 ()
outList (h : t) = i2 (h, t)

-- | The base functor algebra for lists over the shape 'Either () (a, c)',
-- applying @g@ to the head and @f@ to the recursive position.
baseList :: (a -> b) -> (c -> d) -> Either () (a, c) -> Either () (b, d)
baseList g f = id -|- (g >< f)

-- Ana + Cata + Hylo ---------------------------------------------------------------

-- | The recursive step for the list shape, expressed as a map over the
-- base functor that leaves the head untouched and applies @f@ recursively.
recList :: (c -> d) -> Either () (b, c) -> Either () (b, d)
recList f = baseList id f

-- | The anamorphism for lists, unfolding a seed value into a list via a
-- coalgebra @g :: c -> Either () (a, c)@.
anaList :: (c -> Either () (a, c)) -> c -> [a]
anaList g = inList . recList (anaList g) . g

-- | The catamorphism for lists, folding a list through its base functor
-- by applying an algebra @g :: Either () (a, d) -> d@.
cataList :: (Either () (a, d) -> d) -> [a] -> d
cataList g = g . recList (cataList g) . outList

-- | The hylomorphism for lists, combining an anamorphism and a catamorphism
-- into a single pass.
hyloList :: (Either () (b, c) -> c) -> (a -> Either () (b, a)) -> a -> c
hyloList f g = cataList f . anaList g

-- Others --------------------------------------------------------------------------

-- | Right fold over a list. Equivalent to 'foldr', expressed as a catamorphism.
foldrList :: (a -> b -> b) -> b -> [a] -> b
foldrList f i = cataList (const i \/ uncurry f)

-- | Left fold over a list. Equivalent to 'foldl', expressed as a catamorphism.
-- Note: not tail-recursive due to the catamorphism structure.
foldlList :: (b -> a -> b) -> b -> [a] -> b
foldlList f i = cataList (const i \/ uncurry f . swap)

-- | Computes the length of a list as a catamorphism,
-- counting each cons cell.
lenList :: [a] -> Int
lenList = cataList (zero \/ (succ . p2))

-- | Reverses a list as a catamorphism by appending each head to the end.
revList :: [a] -> [a]
revList = cataList (nil \/ aux)
  where
    aux (h, l) = l ++ [h]

-- | Concatenates a list of lists into a single list.
concList :: [[a]] -> [a]
concList = cataList (nil \/ uncurry (++))

-- | Looks up a key in an association list, returning 'Just' the first
-- matching value or 'Nothing' if the key is absent.
lookUpList :: (Eq a) => a -> [(a, b)] -> Maybe b
lookUpList k = cataList (nothing \/ aux k)
  where
    aux t ((a, b), r) = if a == t then Just b else r

-- TODO
-- | Sorts a list by repeatedly inserting each element into its correct position.
insertionSort :: (Ord a) => [a] -> [a]
insertionSort = undefined

-- TODO
-- | Sorts a list using the merge sort algorithm, expressed as a list hylomorphism.
mergeSort :: (Ord a) => [a] -> [a]
mergeSort = undefined

-- | Takes the first @n@ elements of a list, expressed as an anamorphism
-- that stops when either the count reaches zero or the list is exhausted.
takeList :: Int -> [a] -> [a]
takeList = curry aux
  where
    aux = anaList divide
    divide (0, _) = i1 ()
    divide (_, []) = i1 ()
    divide (n + 1, h : t) = i2 (h, (n, t))
    divide (_, _) = error "Invalid input"

-- TODO
-- | Drops the first @n@ elements of a list.
dropList :: Int -> [a] -> [a]
dropList = undefined

-- | Computes the factorial of a natural number as a list hylomorphism:
-- the coalgebra unfolds @n@ into the list @[n, n-1, .., 1]@ and the
-- algebra folds it by multiplying all elements.
facList :: Integer -> Integer
facList = hyloList (const 1 \/ mul) ((id -|- (succ /\ id)) . outNat)

-- | Produces the countdown list @[n, n-1, .., 1]@ from a natural number,
-- expressed as an anamorphism driven by the 'outNat' coalgebra.
countdown :: Integer -> [Integer]
countdown = anaList ((id -|- (succ /\ id)) . outNat)

-- | Computes all prefixes of a list, ordered from shortest to longest.
-- The empty list is always the first prefix.
prefixes :: [a] -> [[a]]
prefixes = cataList (const [[]] \/ aux)
  where
    aux (h, l) = [] : fmap (h :) l

-- | Computes all suffixes of a list, ordered from longest to shortest,
-- expressed as an anamorphism over the list structure.
suffixes :: [a] -> [[a]]
suffixes = anaList ((id -|- (cons /\ p2)) . outList)

-- | List difference. Returns the elements of the first list that do not
-- appear in the second list, preserving order.
diff :: (Eq a) => [a] -> [a] -> [a]
diff = flip aux
  where
    aux l = cataList (nil \/ g l)
    g x (h, l) = if h `elem` x then l else h : l

-- | Splits a list into chunks of size @n@. The last chunk may be shorter
-- if the list length is not a multiple of @n@.
chunksOf :: Int -> [a] -> [[a]]
chunksOf n = anaList (aux n)
  where
    aux _ [] = i1 ()
    aux t x = i2 (splitAt t x)

-- | Checks whether a list contains no repeated elements.
-- Uses a product catamorphism that simultaneously accumulates the visited
-- elements and a boolean flag.
noRepeats :: (Eq a) => [a] -> Bool
noRepeats = p2 . cataList (f \/ (g /\ h))
  where
    f _ = ([], True)
    g (a, (t, _)) = a : t
    h (a, (t, b)) = not (a `elem` t) && b

-- | List concatenation, expressed as a catamorphism.
-- Equivalent to @('++')@.
plusplus :: [a] -> [a] -> [a]
plusplus = cataList (const id \/ uncurry (.)) . fmap (:)

-- | Interleaves two lists into a single list of 'Either' values,
-- tagging elements from the first list with 'Left' and from the second with 'Right'.
join :: ([a], [b]) -> [Either a b]
join (a, b) = fmap i1 a ++ fmap i2 b

-- | Separates a list of 'Either' values into a pair of lists,
-- collecting 'Left' values on the left and 'Right' values on the right.
-- Inverse of 'join'.
sep :: [Either a b] -> ([a], [b])
sep = cataList ((const ([], [])) \/ aux)
  where
    aux (x, (l, r)) = case x of
      Left a -> (a : l, r)
      Right b -> (l, b : r)
