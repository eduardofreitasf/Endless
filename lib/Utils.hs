-- |
-- Module      : Utils
-- Description : Utility helpers used by library examples and algorithms
-- Copyright   : (c) 2026 Eduardo Freitas Fernandes
-- License     : MIT
-- Maintainer  : eduardof.fernandes05@gmail.com
-- Stability   : experimental
--
-- This module provides small utility functions used across the library.
module Utils where

-- | Partition a list according to a predicate.
--
-- The first component contains elements satisfying the predicate,
-- while the second component contains the remaining elements.
part :: (a -> Bool) -> [a] -> ([a], [a])
part _ [] = ([], [])
part p (h : t)
  | p h = let (s, l) = part p t in (h : s, l)
  | otherwise = let (s, l) = part p t in (s, h : l)
