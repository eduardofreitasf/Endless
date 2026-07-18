{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NPlusKPatterns #-}

{- |
Module      : BTree
Description : Binary trees as a datatype with base functor Either () (a, (BTree a, BTree a))
Copyright   : (c) 2026 Eduardo Freitas Fernandes
License     : MIT
Maintainer  : eduardof.fernandes05@gmail.com
Stability   : experimental

This module provides combinators for working with binary trees, viewed
through their base functor 'Either () (a, (BTree a, BTree a))'. The empty
tree corresponds to the left injection and a node to the right injection,
enabling recursive program calculation via anamorphisms, catamorphisms,
and hylomorphisms.
-}
module BTree where

import Algebra
import Data.List (union)
import Utils (part)

-- Datatype definition -------------------------------------------------------------

{- | A binary tree: either 'Empty' or a 'Node' holding a value
and a pair of left and right subtrees.
-}
data BTree a = Empty | Node (a, (BTree a, BTree a))

{- | The constructor of the base functor for binary trees,
mapping 'Either () (a, (BTree a, BTree a))' to 'BTree a'
by sending the left case to 'Empty' and the right case to 'Node'.
-}
inBTree :: Either () (a, (BTree a, BTree a)) -> BTree a
inBTree = const Empty \/ Node

{- | The destructor of the base functor for binary trees,
mapping 'BTree a' to 'Either () (a, (BTree a, BTree a))'
by sending 'Empty' to @Left ()@ and @Node r@ to @Right r@.
-}
outBTree :: BTree a -> Either () (a, (BTree a, BTree a))
outBTree Empty = i1 ()
outBTree (Node r) = i2 r

{- | The base functor algebra for binary trees over the shape
'Either () (a, (c, c))', applying @g@ to the node value
and @f@ to each of the two recursive positions.
-}
baseBTree :: (a -> b) -> (c -> d) -> Either () (a, (c, c)) -> Either () (b, (d, d))
baseBTree g f = id -|- g >< (f >< f)

-- Ana + Cata + Hylo ---------------------------------------------------------------

{- | The recursive step for the binary tree shape, expressed as a map
over the base functor that leaves the node value untouched and
applies @f@ to each of the two recursive subtree positions.
-}
recBTree :: (c -> d) -> Either () (b, (c, c)) -> Either () (b, (d, d))
recBTree f = baseBTree id f

{- | The anamorphism for binary trees, unfolding a seed value into a
'BTree' via a coalgebra @g :: c -> Either () (a, (c, c))@.
The left case produces 'Empty'; the right case produces a 'Node'
whose subtrees are obtained by further unfolding.
-}
anaBTree :: (c -> Either () (a, (c, c))) -> c -> BTree a
anaBTree g = inBTree . recBTree (anaBTree g) . g

{- | The catamorphism for binary trees, folding a 'BTree' through its
base functor by applying an algebra @g :: Either () (b, (d, d)) -> d@.
-}
cataBTree :: (Either () (b, (d, d)) -> d) -> BTree b -> d
cataBTree g = g . recBTree (cataBTree g) . outBTree

{- | The hylomorphism for binary trees, combining an anamorphism and
a catamorphism into a single pass. Equivalent to
@cataBTree f . anaBTree g@, but avoids materialising the intermediate tree.
-}
hyloBTree :: (Either () (b, (c, c)) -> c) -> (a -> Either () (b, (a, a))) -> a -> c
hyloBTree f g = cataBTree f . anaBTree g

-- Others --------------------------------------------------------------------------

instance Functor BTree where
  -- \| Lifts a function over the elements of a 'BTree',
  -- applying it to every node value while preserving the tree shape.
  fmap :: (a -> b) -> BTree a -> BTree b
  fmap f = cataBTree (inBTree . baseBTree f id)

{- | Mirrors a binary tree, swapping the left and right subtrees
at every node, expressed as a catamorphism.
-}
mirrorBTree :: BTree a -> BTree a
mirrorBTree = cataBTree (inBTree . (id -|- id >< swap))

{- | Counts the number of nodes in a binary tree,
expressed as a catamorphism that adds one for each 'Node'.
-}
countBTree :: BTree a -> Int
countBTree = cataBTree (either zero (succ . add . p2))

{- | Computes the depth (height) of a binary tree,
expressed as a catamorphism that takes the maximum of the two
subtree depths and adds one for the current node.
-}
depthBTree :: BTree a -> Int
depthBTree = cataBTree (either zero (succ . umax . p2))

{- | Produces the in-order traversal of a binary tree as a list:
left subtree, then the root value, then the right subtree.
Expressed as a catamorphism.
-}
inordBTree :: BTree a -> [a]
inordBTree = cataBTree aux
 where
  aux = either nil f where f (x, (l, r)) = l ++ [x] ++ r

{- | Produces the pre-order traversal of a binary tree as a list:
the root value, then the left subtree, then the right subtree.
Expressed as a catamorphism.
-}
preordBTree :: BTree a -> [a]
preordBTree = cataBTree aux
 where
  aux = either nil (cons . (id >< conc))

{- | Produces the post-order traversal of a binary tree as a list:
left subtree, then the right subtree, then the root value.
Expressed as a catamorphism.
-}
postordBTree :: BTree a -> [a]
postordBTree = cataBTree aux
 where
  aux = either nil f where f (x, (l, r)) = l ++ r ++ [x]

{- | Sorts a list using quicksort, expressed as a hylomorphism over
'BTree'. The anamorphism @g@ builds a binary search tree by
partitioning each tail around its head; the catamorphism @f@
flattens the tree back via in-order traversal, yielding a sorted list.
No intermediate 'BTree' value is constructed at runtime.
-}
quickSortBTree :: (Ord a) => [a] -> [a]
quickSortBTree = hyloBTree f g
 where
  f = either nil h where h (x, (l, r)) = l ++ [x] ++ r

  g [] = i1 ()
  g (h : t) = i2 (h, sl) where sl = part (< h) t

{- | Computes all root-to-leaf paths of a binary tree as a list of lists.
Paths from the left and right subtrees are merged with 'union'
after prepending the current node value.
-}
traces :: (Eq a) => BTree a -> [[a]]
traces = cataBTree (either (const [[]]) tunion)
 where
  tunion (a, (l, r)) = union (map (a :) l) (map (a :) r)

{- | Returns 'True' if the binary tree is height-balanced, i.e. the
depths of the two subtrees of every node differ by at most one.
-}
isBalancedBTree :: BTree a -> Bool
isBalancedBTree = p1 . balanceDepth

{- | Returns the depth (height) of the binary tree.
Equivalent to 'depthBTree' but computed as part of the same
product catamorphism used by 'isBalancedBTree'.
-}
depthBalanceBTree :: BTree a -> Int
depthBalanceBTree = p2 . balanceDepth

{- | Internal helper shared by 'isBalancedBTree' and 'depthBalanceBTree'.
Returns a pair @(balanced, depth)@ computed in a single catamorphism pass,
where @balanced@ is 'True' iff the tree is height-balanced and @depth@
is the height of the tree.
-}
balanceDepth :: BTree b -> (Bool, Int)
balanceDepth = cataBTree g
 where
  g = (const (True, 1)) \/ (h . (id >< f))
  h (_, ((b1, b2), (d1, d2))) = (b1 && b2 && abs (d1 - d2) <= 1, 1 + max d1 d2)
  f ((b1, d1), (b2, d2)) = ((b1, b2), (d1, d2))

{- | Solves the Tower of Hanoi problem, expressed as a hylomorphism over
'BTree'. The input @(d, n)@ encodes the direction of the first move @d@
and the number of discs @n@. The anamorphism @strategy@ builds the
recursive move tree and the catamorphism @present@ flattens it into the
ordered sequence of moves, where each move is a @(Bool, Integer)@ pair
identifying the disc.
-}
hanoi :: (Bool, Integer) -> [(Bool, Integer)]
hanoi = hyloBTree present strategy
 where
  present = either nil f where f (x, (l, r)) = l ++ [x] ++ r

  strategy (_, 0) = i1 ()
  strategy (d, n + 1) = i2 ((d, n), dup (not d, n))
