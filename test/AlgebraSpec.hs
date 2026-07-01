{-# LANGUAGE ScopedTypeVariables #-}

module AlgebraSpec (tests) where

import Algebra
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding ((><))

tests :: TestTree
tests =
  testGroup
    "Algebra Module Tests"
    [ testGroup
        "Projections & Injections"
        [ testProperty "p1 . (f /\\ g) == f" $ \(Fn f :: Fun Int Int) (Fn g :: Fun Int Int) (x :: Int) ->
            p1 ((f /\ g) x) == f x,
          testProperty "p2 . (f /\\ g) == g" $ \(Fn f :: Fun Int Int) (Fn g :: Fun Int Int) (x :: Int) ->
            p2 ((f /\ g) x) == g x,
          testProperty "i1 injection" $ \(x :: Int) ->
            i1 x == (Left x :: Either Int Bool),
          testProperty "i2 injection" $ \(x :: Int) ->
            i2 x == (Right x :: Either Bool Int)
        ],
      testGroup
        "Product & Coproduct Combinators"
        [ testProperty "Product of functions (><)" $ \(Fn f :: Fun Int Int) (Fn g :: Fun Int Int) (x :: Int) (y :: Int) ->
            (f >< g) (x, y) == (f x, g y),
          testProperty "Coproduct either combinator (\\/)" $ \(Fn f :: Fun Int Int) (Fn g :: Fun Int Int) (e :: Either Int Int) ->
            (f \/ g) e == case e of
              Left x -> f x
              Right y -> g y,
          testProperty "Coproduct of functions (-|-)" $ \(Fn f :: Fun Int Int) (Fn g :: Fun Int Int) (e :: Either Int Int) ->
            (f -|- g) e == case e of
              Left x -> Left (f x)
              Right y -> Right (g y)
        ],
      testGroup
        "McCarthy's Conditional"
        [ testProperty "guard routing" $ \(Fn p :: Fun Int Bool) (x :: Int) ->
            guard p x == if p x then Left x else Right x,
          testProperty "cond branching" $ \(Fn p :: Fun Int Bool) (Fn f :: Fun Int Int) (Fn g :: Fun Int Int) (x :: Int) ->
            cond p f g x == if p x then f x else g x
        ],
      testGroup
        "Exponentiation"
        [ testProperty "ap application" $ \(Fn f :: Fun Int Int) (x :: Int) ->
            ap (f, x) == f x,
          testProperty "rexp currying relation" $ \(Fn f :: Fun Int Int) (Fn g :: Fun Int Int) (x :: Int) ->
            rexp f g x == f (g x),
          testProperty "lexp currying relation" $ \(Fn f :: Fun Int Int) (Fn g :: Fun Int Int) (x :: Int) ->
            lexp f g x == g (f x)
        ],
      testGroup
        "Natural Isomorphisms"
        [ testProperty "swap swap id" $ \(x :: Int, y :: Int) ->
            swap (swap (x, y)) == (x, y),
          testProperty "assocr . assocl == id" $ \(x :: Int, (y :: Int, z :: Int)) ->
            assocr (assocl (x, (y, z))) == (x, (y, z)),
          testProperty "assocl . assocr == id" $ \((x :: Int, y :: Int), z :: Int) ->
            assocl (assocr ((x, y), z)) == ((x, y), z),
          testProperty "coswap swap id" $ \(x :: Either Int Bool) ->
            coswap (coswap x) == x,
          testProperty "coassocr . coassocl == id" $ \(x :: Either Int (Either Bool Int)) ->
            coassocr (coassocl x) == x,
          testProperty "coassocl . coassocr == id" $ \(x :: Either (Either Int Bool) Int) ->
            coassocl (coassocr x) == x,
          testProperty "distr . undistr == id" $ \(x :: Either (Int, Bool) (Int, Int)) ->
            distr (undistr x) == x,
          testProperty "undistr . distr == id" $ \(x :: (Int, Either Bool Int)) ->
            undistr (distr x) == x,
          testProperty "distl . undistl == id" $ \(x :: Either (Bool, Int) (Int, Int)) ->
            distl (undistl x) == x,
          testProperty "undistl . distl == id" $ \(x :: (Either Bool Int, Int)) ->
            undistl (distl x) == x,
          testProperty "subr shift" $ \(x :: Int, (y :: Int, z :: Int)) ->
            subr (x, (y, z)) == (y, (x, z)),
          testProperty "subl shift" $ \((x :: Int, y :: Int), z :: Int) ->
            subl ((x, y), z) == ((x, z), y),
          testProperty "distp product distribution" $ \((a :: Int, b :: Int), (c :: Int, d :: Int)) ->
            distp ((a, b), (c, d)) == ((a, c), (b, d)),
          testProperty "flatr and flatl match" $ \(a :: Int, b :: Int, c :: Int) ->
            flatr (a, (b, c)) == (a, b, c) && flatl ((a, b), c) == (a, b, c),
          testProperty "lambda and colambda isomorphism (colambda . lambda == id)" $ \(Fn f :: Fun Bool Int) ->
            let f' = colambda (lambda f)
             in f' True == f True && f' False == f False,
          testProperty "lambda and colambda isomorphism (lambda . colambda == id)" $ \(x :: (Int, Int)) ->
            lambda (colambda x) == x
        ],
      testGroup
        "Loops"
        [ testCase "while loop termination" $
            while (< 5) (+ (1 :: Int)) (0 :: Int) @?= 5,
          testCase "while loop initial condition false" $
            while (< 5) (+ (1 :: Int)) (10 :: Int) @?= 10
        ]
    ]
