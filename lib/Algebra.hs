-- |
-- Module      : Algebra
-- Description : Core functional combinators for program calculation
-- Copyright   : (c) 2026 Eduardo Freitas Fernandes
-- License     : MIT
-- Maintainer  : eduardof.fernandes05@gmail.com
-- Stability   : experimental
--
-- This module provides a collection of fundamental functional operators
-- used throughout the library, exposing the structure of products, coproducts,
-- and exponentials.
module Algebra
  ( -- * Projections and Injections
    p1,
    p2,
    i1,
    i2,

    -- * Product combinators
    (/\),
    (><),

    -- * Coproduct combinators
    (\/),
    (-|-),

    -- * McCarthy's Conditional
    guard,
    cond,

    -- * Exponentiation
    ap,
    rexp,
    lexp,

    -- * Natural Isomorphisms
    swap,
    assocr,
    assocl,
    coswap,
    coassocr,
    coassocl,
    distr,
    distl,
    undistr,
    undistl,
    subr,
    subl,
    cosubr,
    cosubl,
    distp,
    dists,
    flatr,
    flatl,
    br,
    bl,
    lambda,
    colambda,

    -- * Basic Functions
    dup,
    join,
    (!),
    zero,
    one,
    nil,
    cons,
    add,
    mul,
    conc,
    umax,
    true,
    false,
    nothing,
    nat0,

    -- * Iteration
    while,
  )
where

-- Projections and Injections -----------------------------------------------------------

-- | First projection of a product. Extracts the first component of a pair.
p1 :: (a, b) -> a
p1 = fst

-- | Second projection of a product. Extracts the second component of a pair.
p2 :: (a, b) -> b
p2 = snd

-- | Left injection into a coproduct. Embeds a value into the left side of an 'Either'.
i1 :: a -> Either a b
i1 = Left

-- | Right injection into a coproduct. Embeds a value into the right side of an 'Either'.
i2 :: b -> Either a b
i2 = Right

-- Product -----------------------------------------------------------------------------

infix 6 /\

-- | The split combinator. Passes one input into two functions, pairing their outputs.
(/\) :: (a -> b) -> (a -> c) -> a -> (b, c)
(/\) f g x = (f x, g x)

infix 7 ><

-- | Product of two functions. Applies both functions component-wise to a pair.
(><) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
(><) f g = (f . p1) /\ (g . p2)

-- Coproduct --------------------------------------------------------------------------

infix 4 \/

-- | The either combinator. Folds a coproduct by applying the appropriate function.
(\/) :: (a -> c) -> (b -> c) -> Either a b -> c
(\/) = either

infix 5 -|-

-- | Coproduct of functions. Applies one of two functions depending on the coproduct value.
(-|-) :: (a -> b) -> (c -> d) -> Either a c -> Either b d
(-|-) f g = (i1 . f) \/ (i2 . g)

-- McCarthy's Conditional --------------------------------------------------------------

-- | Guarded branching. Routes a value according to a predicate.
guard :: (a -> Bool) -> a -> Either a a
guard p x = if p x then i1 x else i2 x

-- | McCarthy's Conditional. Selects between two functions based on a predicate.
cond :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
cond p f g = (f \/ g) . guard p

-- Exponentiation ----------------------------------------------------------------------

-- | Application of a function. Applies a function to its argument, packaged as a product.
ap :: (a -> b, a) -> b
ap = uncurry ($)

-- | Right Exponential composition. Lifts composition through an exponential.
rexp :: (b -> c) -> (a -> b) -> a -> c
rexp f = curry (f . ap)

-- | Left Exponential composition. Lifts composition through an exponential.
lexp :: (a -> b) -> (b -> c) -> a -> c
lexp = flip rexp

-- Natural Isomorphisms ----------------------------------------------------------------

-- | Symmetry of the product. Swaps the components of a pair.
swap :: (a, b) -> (b, a)
swap = p2 /\ p1

-- | Right associativity of the product. Reassociates a nested product to the right.
assocr :: ((a, b), c) -> (a, (b, c))
assocr = (p1 . p1) /\ (p2 >< id)

-- | Left associativity of the product. Reassociates a nested product to the left.
assocl :: (a, (b, c)) -> ((a, b), c)
assocl = (id >< p1) /\ (p2 . p2)

-- | Symmetry of the coproduct. Swaps the sides of a sum type.
coswap :: Either a b -> Either b a
coswap = i2 \/ i1

-- | Right associativity of the coproduct. Reassociates a nested coproduct to the right.
coassocr :: Either (Either a b) c -> Either a (Either b c)
coassocr = (id -|- i1) \/ (i2 . i2)

-- | Left associativity of the coproduct. Reassociates a nested coproduct to the left.
coassocl :: Either b (Either a c) -> Either (Either b a) c
coassocl = (i1 . i1) \/ (i2 -|- id)

-- | Right distributivity of product over coproduct.
distr :: (b, Either c a) -> Either (b, c) (b, a)
distr = (swap -|- swap) . distl . swap

-- | Left distributivity of product over coproduct.
distl :: (Either c a, b) -> Either (c, b) (a, b)
distl = uncurry (curry i1 \/ curry i2)

-- | Undistribute a coproduct of products (right). Inverse of right distributivity.
undistr :: Either (a, b) (a, c) -> (a, Either b c)
undistr = (id >< i1) \/ (id >< i2)

-- | Undistribute a coproduct of products (left). Inverse of left distributivity.
undistl :: Either (b, c) (a, c) -> (Either b a, c)
undistl = (i1 >< id) \/ (i2 >< id)

-- | Shifts the first element to the right of a nested pair.
subr :: (a, (b, c)) -> (b, (a, c))
subr = (p1 . p2) /\ (id >< p2)

-- | Shifts the last element to the left of a nested pair.
subl :: ((a, b), c) -> ((a, c), b)
subl = (p1 >< id) /\ (p2 . p1)

-- | Shifts an option to the right of a nested sum.
cosubr :: Either a (Either b c) -> Either b (Either a c)
cosubr = coassocr . (coswap -|- id) . coassocl

-- | Shifts an option to the left of a nested sum.
cosubl :: Either (Either a b) c -> Either (Either a c) b
cosubl = coassocl . (id -|- coswap) . coassocr

-- | The product distribution combinator.
distp :: ((c, d), (a, b)) -> ((c, a), (d, b))
distp = (p1 >< p1) /\ (p2 >< p2)

-- | The sum distribution combinator.
dists :: (Either a b, Either c d) -> Either (Either (a, c) (a, d)) (Either (b, c) (b, d))
dists = (distr -|- distr) . distl

-- | Flatten a right-associated product. Converts a nested product into a triple.
flatr :: (a, (b, c)) -> (a, b, c)
flatr (a, (b, c)) = (a, b, c)

-- | Flatten a left-associated product. Converts a nested product into a triple.
flatl :: ((a, b), c) -> (a, b, c)
flatl ((b, c), d) = (b, c, d)

-- | Bang on the right. Introduces the terminal object on the right.
br :: a -> (a, ())
br = id /\ (!)

-- | Bang on the left. Introduces the terminal object on the left.
bl :: a -> ((), a)
bl = (!) /\ id

-- | Converts a function on booleans into a product.
lambda :: (Bool -> a) -> (a, a)
lambda f = (f False, f True)

-- | Converts a product into a function on booleans.
colambda :: (a, a) -> Bool -> a
colambda (a, _) False = a
colambda (_, b) True = b

-- Basic Functions ---------------------------------------------------------------------

-- | Diagonal of the product. Duplicates a value into both components of a product.
dup :: a -> (a, a)
dup = id /\ id

-- | Codiagonal of the coproduct. Eliminates a coproduct by identifying both sides.
join :: Either a a -> a
join = id \/ id

-- | Bang operator. Maps every value to the terminal object.
(!) :: a -> ()
(!) = const ()

-- | Constant zero. Ignores its argument and returns @0@.
zero :: a -> Int
zero = const 0

-- | Constant one. Ignores its argument and returns @1@.
one :: a -> Int
one = const 1

-- | Empty list constructor. Produces the empty list, ignoring its argument.
nil :: a -> [b]
nil = const []

-- | List constructor. Prepends an element to a list.
cons :: (a, [a]) -> [a]
cons = uncurry (:)

-- | Addition as a binary operator on products. Adds the components of a pair.
add :: (Num a) => (a, a) -> a
add = uncurry (+)

-- | Multiplication as a binary operator on products. Multiplies the components of a pair.
mul :: (Num a) => (a, a) -> a
mul = uncurry (*)

-- | List concatenation. Concatenates two lists.
conc :: ([a], [a]) -> [a]
conc = uncurry (++)

-- | Binary maximum. Returns the maximum of two values.
umax :: (Ord a) => (a, a) -> a
umax = uncurry max

-- | Constant true. Ignores its argument and returns 'True'.
true :: a -> Bool
true = const True

-- | Constant false. Ignores its argument and returns 'False'.
false :: a -> Bool
false = const False

-- | Constant 'Nothing'. Ignores its argument and returns 'Nothing'.
nothing :: a -> Maybe b
nothing = const Nothing

-- | The natural numbers. Infinite list of natural numbers starting from zero.
nat0 :: [Integer]
nat0 = [0 ..]

-- Loops -------------------------------------------------------------------------------

-- | While loop definition. Point-free representation of a while loop.
while :: (a -> Bool) -> (a -> a) -> a -> a
while p f = join . ((while p f . f) -|- id) . guard p
