{-# LANGUAGE NPlusKPatterns #-}

-- |
-- Module      : Maybe
-- Description : Maybe as a datatype with base functor Either () a
-- Copyright   : (c) 2026 Eduardo Freitas Fernandes
-- License     : MIT
-- Maintainer  : eduardof.fernandes05@gmail.com
-- Stability   : experimental
--
-- This module provides simple helpers for working with the standard Maybe
-- datatype, viewed through its base functor 'Either () a'.
module Maybe where

import Algebra
  ( cond,
    false,
    i1,
    i2,
    nil,
    nothing,
    true,
    (-|-),
    (\/),
  )

-- Datatype definition -------------------------------------------------------------

-- The Maybe datatype is already defined in Haskell
-- data Maybe a = Nothing | Just a

-- | The constructor of the base functor for 'Maybe', mapping 'Either () a' to 'Maybe a'.
inMaybe :: Either () a -> Maybe a
inMaybe = nothing \/ Just

-- | The destructor of the base functor for 'Maybe', mapping 'Maybe a' to 'Either () a'.
outMaybe :: Maybe a -> Either () a
outMaybe Nothing = i1 ()
outMaybe (Just a) = i2 a

-- | The base functor algebra for 'Maybe' over the shape 'Either () a'.
baseMaybe :: (a -> b) -> p -> Either () a -> Either () b
baseMaybe g _ = id -|- g

-- Ana + Cata + Hylo ---------------------------------------------------------------

-- | The recursive step for the 'Maybe' shape, expressed as an algebra over 'Either () a'.
recMaybe :: p -> Either () a -> Either () a
recMaybe f = baseMaybe id f

-- | The anamorphism for 'Maybe', unfolding a value into the 'Either ()' shape.
anaMaybe :: (a -> Either () b) -> a -> Maybe b
anaMaybe g = inMaybe . recMaybe (anaMaybe g) . g

-- | The catamorphism for 'Maybe', folding a 'Maybe' value through its base functor.
cataMaybe :: (Either () a -> b) -> Maybe a -> b
cataMaybe g = g . recMaybe (cataMaybe g) . outMaybe

-- | The hylomorphism for 'Maybe', combining an anamorphism and a catamorphism.
hyloMaybe :: (Either () a -> d) -> (c -> Either () a) -> c -> d
hyloMaybe f g = cataMaybe f . anaMaybe g

-- Others --------------------------------------------------------------------------

-- | Lifts a function through 'Maybe'.
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f = cataMaybe (inMaybe . baseMaybe f id)

-- | Keeps a value only when a predicate holds.
filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe p = cataMaybe (nothing \/ (cond p Just nothing))

-- | Extracts the contained value or returns a default.
fromMaybe :: a -> Maybe a -> a
fromMaybe d = cataMaybe ((const d) \/ id)

-- | Converts a 'Maybe' value to a list with zero or one elements.
maybeToList :: Maybe a -> [a]
maybeToList = cataMaybe (nil \/ (: []))

-- | Tests whether a 'Maybe' value contains a value.
isJust :: Maybe a -> Bool
isJust = cataMaybe (false \/ true)

-- | Flattens a nested 'Maybe'.
joinMaybe :: Maybe (Maybe a) -> Maybe a
joinMaybe = cataMaybe (nothing \/ id)

-- | Returns 'Nothing' when a condition fails; otherwise wraps the value.
guardMaybe :: (a -> Bool) -> a -> Maybe a
guardMaybe p = cond p Just nothing

-- | A conditional transformation returning 'Just' the result when the predicate holds, and 'Nothing' otherwise.
totMaybe :: (a -> b) -> (a -> Bool) -> a -> Maybe b
totMaybe f p = cond p (return . f) nothing
