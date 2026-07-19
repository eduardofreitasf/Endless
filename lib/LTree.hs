{-# LANGUAGE NPlusKPatterns #-}

{- |
Module      : LTree
Description : Leaf trees as a datatype with base functor Either a (LTree a, LTree a)
Copyright   : (c) 2026 Eduardo Freitas Fernandes
License     : MIT
Maintainer  : eduardof.fernandes05@gmail.com
Stability   : experimental

This module provides combinators for working with leaf-labelled binary trees,
viewed through their base functor 'Either a (LTree a, LTree a)'. A leaf holds
a single value (left injection) and an internal node is an unlabelled fork of
two subtrees (right injection), enabling recursive program calculation via
anamorphisms, catamorphisms, and hylomorphisms.
-}
module LTree where

import Algebra

-- Datatype definition -------------------------------------------------------------

{- | A leaf-labelled binary tree: either a 'Leaf' holding a single value
or a 'Fork' of two subtrees with no label at the internal node.
-}
data LTree a = Leaf a | Fork (LTree a, LTree a)

{- | The constructor of the base functor for leaf trees,
mapping 'Either a (LTree a, LTree a)' to 'LTree a'
by sending a value to a 'Leaf' and a pair of subtrees to a 'Fork'.
-}
inLTree :: Either a (LTree a, LTree a) -> LTree a
inLTree = Leaf \/ Fork

{- | The destructor of the base functor for leaf trees,
mapping 'LTree a' to 'Either a (LTree a, LTree a)'
by sending @Leaf x@ to @Left x@ and @Fork f@ to @Right f@.
-}
outLTree :: LTree a -> Either a (LTree a, LTree a)
outLTree (Leaf x) = i1 x
outLTree (Fork f) = i2 f

{- | The base functor algebra for leaf trees over the shape 'Either a (c, c)',
applying @g@ to the leaf value and @f@ to each of the two recursive positions.
-}
baseLTree :: (a -> b) -> (c -> d) -> Either a (c, c) -> Either b (d, d)
baseLTree g f = g -|- (f >< f)

-- Ana + Cata + Hylo ---------------------------------------------------------------

{- | The recursive step for the leaf tree shape, expressed as a map
over the base functor that leaves the leaf value untouched and
applies @f@ to each of the two recursive subtree positions.
-}
recLTree :: (c -> d) -> Either b (c, c) -> Either b (d, d)
recLTree f = baseLTree id f

{- | The anamorphism for leaf trees, unfolding a seed value into an
'LTree' via a coalgebra @g :: c -> Either a (c, c)@.
The left case produces a 'Leaf'; the right case produces a 'Fork'
whose subtrees are obtained by further unfolding.
-}
anaLTree :: (c -> Either a (c, c)) -> c -> LTree a
anaLTree g = inLTree . recLTree (anaLTree g) . g

{- | The catamorphism for leaf trees, folding an 'LTree' through its
base functor by applying an algebra @g :: Either b (d, d) -> d@.
-}
cataLTree :: (Either b (d, d) -> d) -> LTree b -> d
cataLTree g = g . recLTree (cataLTree g) . outLTree

{- | The hylomorphism for leaf trees, combining an anamorphism and
a catamorphism into a single pass. Equivalent to
@cataLTree f . anaLTree g@, but avoids materialising the intermediate tree.
-}
hyloLTree :: (Either b (c, c) -> c) -> (a -> Either b (a, a)) -> a -> c
hyloLTree f g = cataLTree f . anaLTree g

-- Others --------------------------------------------------------------------------

instance Functor LTree where
  -- \| Lifts a function over the leaf values of an 'LTree',
  -- applying it to every 'Leaf' while preserving the tree shape.
  fmap f = cataLTree (inLTree . baseLTree f id)

{- | Mirrors a leaf tree, swapping the two subtrees of every 'Fork',
expressed as a catamorphism.
-}
mirrorLTree :: LTree a -> LTree a
mirrorLTree = cataLTree (inLTree . (id -|- swap))

{- | Counts the number of leaves in a leaf tree,
expressed as a catamorphism that returns @1@ at each 'Leaf'
and sums the counts of the two subtrees at each 'Fork'.
-}
countLTree :: LTree a -> Int
countLTree = cataLTree (either one add)

{- | Computes the depth (height) of a leaf tree,
expressed as a catamorphism that returns @1@ at each 'Leaf'
and takes the maximum of the two subtree depths plus one at each 'Fork'.
-}
depthLTree :: LTree a -> Int
depthLTree = cataLTree (either one (succ . umax))

{- | Collects all leaf values of a leaf tree into a list,
preserving left-to-right order. Expressed as a catamorphism that
wraps each 'Leaf' value in a singleton list and concatenates
the results of the two subtrees at each 'Fork'.
-}
tipsLTree :: LTree a -> [a]
tipsLTree = cataLTree (either singl conc)
 where
  singl x = [x]
