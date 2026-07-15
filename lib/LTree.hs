{-# LANGUAGE NPlusKPatterns #-}

-- |
-- Module      : LTree
-- Description : TODO: brief description
-- Copyright   : (c) 2026 Eduardo Freitas Fernandes
-- License     : MIT
-- Maintainer  : eduardof.fernandes05@gmail.com
-- Stability   : experimental
--
-- TODO: detailed description
module LTree where

import Algebra

-- Datatype definition -------------------------------------------------------------

data LTree a = Leaf a | Fork (LTree a, LTree a)

inLTree :: Either a (LTree a, LTree a) -> LTree a
inLTree = Leaf \/ Fork

outLTree :: LTree a -> Either a (LTree a, LTree a)
outLTree (Leaf x) = i1 x
outLTree (Fork f) = i2 f

baseLTree :: (a -> b) -> (c -> d) -> Either a (c, c) -> Either b (d, d)
baseLTree g f = g -|- (f >< f)

-- Ana + Cata + Hylo ---------------------------------------------------------------

recLTree :: (c -> d) -> Either b (c, c) -> Either b (d, d)
recLTree f = baseLTree id f

anaLTree :: (c -> Either a (c, c)) -> c -> LTree a
anaLTree g = inLTree . recLTree (anaLTree g) . g

cataLTree :: (Either b (d, d) -> d) -> LTree b -> d
cataLTree g = g . recLTree (cataLTree g) . outLTree

hyloLTree :: (Either b (c, c) -> c) -> (a -> Either b (a, a)) -> a -> c
hyloLTree f g = cataLTree f . anaLTree g

-- Others --------------------------------------------------------------------------