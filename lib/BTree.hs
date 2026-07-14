{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NPlusKPatterns #-}

-- |
-- Module      : BTree
-- Description : Binary trees as a datatype with base functor Either () (a, (BTree a, BTree a))
-- Copyright   : (c) 2026 Eduardo Freitas Fernandes
-- License     : MIT
-- Maintainer  : eduardof.fernandes05@gmail.com
-- Stability   : experimental
--
-- This module provides combinators for working with binary trees, viewed
-- through their base functor 'Either () (a, (BTree a, BTree a))'. The empty
-- tree corresponds to the left injection and a node to the right injection,
-- enabling recursive program calculation via anamorphisms, catamorphisms,
-- and hylomorphisms.
module BTree where

import Algebra
import Data.List (union)
import Utils (part)

-- Datatype definition -------------------------------------------------------------

data BTree a = Empty | Node (a, (BTree a, BTree a))

inBTree :: Either () (a, (BTree a, BTree a)) -> BTree a
inBTree = const Empty \/ Node

outBTree :: BTree a -> Either () (a, (BTree a, BTree a))
outBTree Empty = i1 ()
outBTree (Node r) = i2 r

baseBTree :: (a -> b) -> (c -> d) -> Either () (a, (c, c)) -> Either () (b, (d, d))
baseBTree g f = id -|- g >< (f >< f)

-- Ana + Cata + Hylo ---------------------------------------------------------------

recBTree :: (c -> d) -> Either () (b, (c, c)) -> Either () (b, (d, d))
recBTree f = baseBTree id f

anaBTree :: (c -> Either () (a, (c, c))) -> c -> BTree a
anaBTree g = inBTree . recBTree (anaBTree g) . g

cataBTree :: (Either () (b, (d, d)) -> d) -> BTree b -> d
cataBTree g = g . recBTree (cataBTree g) . outBTree

hyloBTree :: (Either () (b, (c, c)) -> c) -> (a -> Either () (b, (a, a))) -> a -> c
hyloBTree f g = cataBTree f . anaBTree g

-- Others --------------------------------------------------------------------------

instance Functor BTree where
  fmap :: (a -> b) -> BTree a -> BTree b
  fmap f = cataBTree (inBTree . baseBTree f id)

mirrorBTree :: BTree a -> BTree a
mirrorBTree = cataBTree (inBTree . (id -|- id >< swap))

countBTree :: BTree a -> Int
countBTree = cataBTree (either zero (succ . add . p2))

depthBTree :: BTree a -> Int
depthBTree = cataBTree (either zero (succ . umax . p2))

inordBTree :: BTree a -> [a]
inordBTree = cataBTree aux
  where
    aux = either nil f where f (x, (l, r)) = l ++ [x] ++ r

preordBTree :: BTree a -> [a]
preordBTree = cataBTree aux
  where
    aux = either nil (cons . (id >< conc))

postordBTree :: BTree a -> [a]
postordBTree = cataBTree aux
  where
    aux = either nil f where f (x, (l, r)) = l ++ r ++ [x]

quickSortBTree :: (Ord a) => [a] -> [a]
quickSortBTree = hyloBTree f g
  where
    f = either nil h where h (x, (l, r)) = l ++ [x] ++ r

    g [] = i1 ()
    g (h : t) = i2 (h, sl) where sl = part (< h) t

traces :: (Eq a) => BTree a -> [[a]]
traces = cataBTree (either (const [[]]) tunion)
  where
    tunion (a, (l, r)) = union (map (a :) l) (map (a :) r)

isBalancedBTree :: BTree a -> Bool
isBalancedBTree = p1 . balanceDepth

depthBalanceBTree :: BTree a -> Int
depthBalanceBTree = p2 . balanceDepth

balanceDepth :: BTree b -> (Bool, Int)
balanceDepth = cataBTree g
  where
    g = (const (True, 1)) \/ (h . (id >< f))
    h (_, ((b1, b2), (d1, d2))) = (b1 && b2 && abs (d1 - d2) <= 1, 1 + max d1 d2)
    f ((b1, d1), (b2, d2)) = ((b1, b2), (d1, d2))

hanoi :: (Bool, Integer) -> [(Bool, Integer)]
hanoi = hyloBTree present strategy
  where
    present = either nil f where f (x, (l, r)) = l ++ [x] ++ r

    strategy (_, 0) = i1 ()
    strategy (d, n + 1) = i2 ((d, n), dup (not d, n))
