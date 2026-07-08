{-# LANGUAGE ScopedTypeVariables #-}

module NatSpec (tests) where

import Algebra
import Nat
import Prelude hiding (exp, replicate)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Nat Module Tests"
    [ testGroup
        "Base Functor & Isomorphisms"
        [ testCase "inNat Left (zero)" $
            inNat (Left ()) @?= 0,
          testCase "inNat Right (successor)" $
            inNat (Right 4) @?= 5,
          testCase "outNat zero" $
            outNat (0 :: Integer) @?= Left (),
          testCase "outNat successor" $
            outNat (5 :: Integer) @?= Right 4,
          testProperty "inNat . outNat == id" $ \(NonNegative (n :: Int)) ->
            inNat (outNat n) == n,
          testProperty "outNat . inNat == id (Right branch)" $ \(NonNegative (n :: Int)) ->
            outNat (inNat (Right n) :: Int) == Right n
        ],
      testGroup
        "Recursion Schemes (Ana, Cata, Hylo)"
        [ testProperty "cataNat with successor algebra recovers the value" $
            \(NonNegative (n :: Integer)) ->
              cataNat (const (0 :: Int) \/ succ) n == fromIntegral n,
          testProperty "anaNat . outNat == id (as Int)" $
            \(NonNegative (n :: Integer)) ->
              anaNat outNat n == fromIntegral n,
          testProperty "hyloNat identity" $
            \(NonNegative (n :: Integer)) ->
              hyloNat (const (0 :: Int) \/ succ) outNat n == fromIntegral n
        ],
      testGroup
        "for"
        [ testCase "for applies function n times" $
            for (+ 1) (0 :: Int) (5 :: Integer) @?= 5,
          testCase "for zero iterations returns initial value" $
            for (+ 1) (42 :: Int) (0 :: Integer) @?= 42,
          testProperty "for (+1) 0 n == n" $
            \(NonNegative (n :: Integer)) ->
              for (+ 1) (0 :: Int) n == fromIntegral n
        ],
      testGroup
        "addition"
        [ testCase "addition 3 4 == 7" $
            addition (3 :: Int) (4 :: Integer) @?= 7,
          testCase "addition 0 n == n" $
            addition (0 :: Int) (10 :: Integer) @?= 0 + 10,
          testProperty "addition a n == a + n" $
            \(NonNegative (a :: Int)) (NonNegative (n :: Integer)) ->
              addition a n == a + fromIntegral n
        ],
      testGroup
        "multiplication"
        [ testCase "multiplication 3 4 == 12" $
            multiplication 3 (4 :: Integer) @?= 12,
          testCase "multiplication 0 n == 0" $
            multiplication 0 (7 :: Integer) @?= 0,
          testCase "multiplication n 0 == 0" $
            multiplication 5 (0 :: Integer) @?= 0,
          testProperty "multiplication a n == a * n" $
            \(NonNegative (a :: Int)) (NonNegative (n :: Integer)) ->
              multiplication a n == a * fromIntegral n
        ],
      testGroup
        "exp"
        [ testCase "exp 2 0 == 1" $
            exp 2 (0 :: Integer) @?= 1,
          testCase "exp 2 3 == 8" $
            exp 2 (3 :: Integer) @?= 8,
          testCase "exp 3 4 == 81" $
            exp 3 (4 :: Integer) @?= 81,
          testProperty "exp a n == a ^ n" $
            \(NonNegative (a :: Int)) (NonNegative (n :: Integer)) ->
              exp a n == a ^ (fromIntegral n :: Int)
        ],
      testGroup
        "squareOf"
        [ testCase "squareOf 0 == 0" $
            squareOf 0 @?= 0,
          testCase "squareOf 1 == 1" $
            squareOf 1 @?= 1,
          testCase "squareOf 5 == 25" $
            squareOf 5 @?= 25,
          testProperty "squareOf n == n * n" $
            \(NonNegative (n :: Integer)) ->
              squareOf n == fromIntegral (n * n)
        ],
      testGroup
        "facNat"
        [ testCase "facNat 0 == 1" $
            facNat 0 @?= 1,
          testCase "facNat 1 == 1" $
            facNat 1 @?= 1,
          testCase "facNat 5 == 120" $
            facNat 5 @?= 120,
          testCase "facNat 10 == 3628800" $
            facNat 10 @?= 3628800
        ],
      testGroup
        "idiv"
        [ testCase "idiv 10 2 == 5" $
            idiv 10 2 @?= 5,
          testCase "idiv 7 2 == 3 (integer division)" $
            idiv 7 2 @?= 3,
          testCase "idiv 0 5 == 0" $
            idiv 0 5 @?= 0,
          testProperty "idiv n 1 == n" $
            \(NonNegative (n :: Integer)) ->
              idiv n 1 == fromIntegral n,
          testProperty "idiv (a*b) a == b" $
            \(Positive (a :: Integer)) (NonNegative (b :: Integer)) ->
              idiv (a * b) a == fromIntegral b
        ],
      testGroup
        "bubbleSort"
        [ testCase "bubbleSort empty list" $
            bubbleSort ([] :: [Int]) @?= [],
          testCase "bubbleSort singleton" $
            bubbleSort [1 :: Int] @?= [1],
          testCase "bubbleSort already sorted" $
            bubbleSort [1, 2, 3, 4 :: Int] @?= [1, 2, 3, 4],
          testCase "bubbleSort reverse sorted" $
            bubbleSort [4, 3, 2, 1 :: Int] @?= [1, 2, 3, 4],
          testCase "bubbleSort mixed" $
            bubbleSort [3, 1, 4, 1, 5, 9, 2, 6 :: Int] @?= [1, 1, 2, 3, 4, 5, 6, 9],
          testProperty "bubbleSort result is sorted" $
            \(xs :: [Int]) ->
              let sorted = bubbleSort xs
               in and (zipWith (<=) sorted (tail sorted))
                    || length sorted <= 1,
          testProperty "bubbleSort preserves length" $
            \(xs :: [Int]) ->
              length (bubbleSort xs) == length xs
        ],
      testGroup
        "fibNat"
        [ testCase "fibNat 0 == 1" $
            fibNat 0 @?= 1,
          testCase "fibNat 1 == 1" $
            fibNat 1 @?= 1,
          testCase "fibNat 2 == 2" $
            fibNat 2 @?= 2,
          testCase "fibNat 3 == 3" $
            fibNat 3 @?= 3,
          testCase "fibNat 6 == 13" $
            fibNat 6 @?= 13,
          testCase "fibNat 9 == 55" $
            fibNat 9 @?= 55,
          testProperty "fibNat (n+2) == fibNat (n+1) + fibNat n" $
            \(NonNegative (n :: Integer)) ->
              n < 20
                ==> fibNat (n + 2) == fibNat (n + 1) + fibNat n
        ],
      testGroup
        "replicate"
        [ testCase "replicate 0 times is id" $
            replicate (+ 1) (0 :: Integer) (0 :: Int) @?= 0,
          testCase "replicate (+1) 5 0 == 5" $
            replicate (+ 1) (5 :: Integer) (0 :: Int) @?= 5,
          testCase "replicate (*2) 3 1 == 8" $
            replicate (* 2) (3 :: Integer) (1 :: Int) @?= 8,
          testProperty "replicate (+1) n x == x + n" $
            \(NonNegative (n :: Integer)) (x :: Int) ->
              replicate (+ 1) n x == x + fromIntegral n
        ]
    ]
