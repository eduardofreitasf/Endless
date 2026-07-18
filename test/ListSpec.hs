{-# LANGUAGE ScopedTypeVariables #-}

module ListSpec (tests) where

import Algebra hiding (join)
import List
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "List Module Tests"
    [ testGroup
        "Base Functor & Isomorphisms"
        [ testCase "inList Left (nil)" $
            inList (Left ()) @?= ([] :: [Int])
        , testCase "inList Right (cons)" $
            inList (Right (1, [2, 3])) @?= [1, 2, 3 :: Int]
        , testCase "outList empty" $
            outList ([] :: [Int]) @?= Left ()
        , testCase "outList non-empty" $
            outList [1, 2, 3 :: Int] @?= Right (1, [2, 3])
        , testProperty "inList . outList == id" $ \(xs :: [Int]) ->
            inList (outList xs) == xs
        , testProperty "outList . inList == id" $ \(xs :: Either () (Int, [Int])) ->
            outList (inList xs) == xs
        ]
    , testGroup
        "Recursion Schemes (Ana, Cata, Hylo)"
        [ testProperty "cataList with nil/cons algebra recovers the list" $ \(xs :: [Int]) ->
            cataList inList xs == xs
        , testProperty "anaList outList == id" $ \(xs :: [Int]) ->
            anaList outList xs == xs
        , testProperty "hyloList identity" $ \(xs :: [Int]) ->
            hyloList inList outList xs == xs
        ]
    , testGroup
        "foldrList"
        [ testCase "foldrList sum" $
            foldrList (+) 0 [1, 2, 3 :: Int] @?= 6
        , testCase "foldrList empty" $
            foldrList (+) 0 ([] :: [Int]) @?= 0
        , testCase "foldrList cons == id" $
            foldrList (:) [] [1, 2, 3 :: Int] @?= [1, 2, 3]
        , testProperty "foldrList == foldr" $ \(Fn f :: Fun (Int, Int) Int) (z :: Int) (xs :: [Int]) ->
            foldrList (curry f) z xs == foldr (curry f) z xs
        ]
    , testGroup
        "foldlList"
        [ testCase "foldlList sum" $
            foldlList (+) 0 [1, 2, 3 :: Int] @?= 6
        , testCase "foldlList empty" $
            foldlList (+) 0 ([] :: [Int]) @?= 0
        , testProperty "foldlList (+) 0 == sum" $ \(xs :: [Int]) ->
            foldlList (+) 0 xs == sum xs
        ]
    , testGroup
        "lenList"
        [ testCase "lenList empty" $
            lenList ([] :: [Int]) @?= 0
        , testCase "lenList singleton" $
            lenList [42 :: Int] @?= 1
        , testCase "lenList [1..5]" $
            lenList [1 .. 5 :: Int] @?= 5
        , testProperty "lenList == length" $ \(xs :: [Int]) ->
            lenList xs == length xs
        ]
    , testGroup
        "revList"
        [ testCase "revList empty" $
            revList ([] :: [Int]) @?= []
        , testCase "revList singleton" $
            revList [1 :: Int] @?= [1]
        , testCase "revList [1,2,3]" $
            revList [1, 2, 3 :: Int] @?= [3, 2, 1]
        , testProperty "revList . revList == id" $ \(xs :: [Int]) ->
            revList (revList xs) == xs
        , testProperty "revList == reverse" $ \(xs :: [Int]) ->
            revList xs == reverse xs
        ]
    , testGroup
        "concList"
        [ testCase "concList empty" $
            concList ([] :: [[Int]]) @?= []
        , testCase "concList singleton" $
            concList [[1, 2], [3, 4 :: Int]] @?= [1, 2, 3, 4]
        , testProperty "concList == concat" $ \(xss :: [[Int]]) ->
            concList xss == concat xss
        ]
    , testGroup
        "lookUpList"
        [ testCase "lookUpList found" $
            lookUpList 'b' [('a', 1), ('b', 2), ('c', 3)] @?= Just 2
        , testCase "lookUpList first match" $
            lookUpList 'a' [('a', 1), ('a', 99)] @?= Just 1
        , testCase "lookUpList not found" $
            lookUpList 'z' [('a', 1), ('b', 2)] @?= Nothing
        , testCase "lookUpList empty" $
            lookUpList (0 :: Int) [] @?= (Nothing :: Maybe Int)
        ]
    , testGroup
        "takeList"
        [ testCase "takeList 0" $
            takeList 0 [1, 2, 3 :: Int] @?= []
        , testCase "takeList n == length" $
            takeList 3 [1, 2, 3 :: Int] @?= [1, 2, 3]
        , testCase "takeList beyond length" $
            takeList 10 [1, 2 :: Int] @?= [1, 2]
        , testCase "takeList empty list" $
            takeList 5 ([] :: [Int]) @?= []
        , testProperty "takeList n == take n" $ \(NonNegative n) (xs :: [Int]) ->
            takeList n xs == take n xs
        ]
    , testGroup
        "dropList"
        [ testCase "dropList 0" $
            dropList 0 [1, 2, 3 :: Int] @?= [1, 2, 3]
        , testCase "dropList n == length" $
            dropList 3 [1, 2, 3 :: Int] @?= []
        , testCase "dropList beyond length" $
            dropList 10 [1, 2 :: Int] @?= []
        , testCase "dropList empty list" $
            dropList 5 ([] :: [Int]) @?= []
        , testProperty "dropList n == drop n" $ \(NonNegative n) (xs :: [Int]) ->
            dropList n xs == drop n xs
        , testProperty "takeList n xs ++ dropList n xs == xs" $ \(NonNegative n) (xs :: [Int]) ->
            takeList n xs ++ dropList n xs == xs
        ]
    , testGroup
        "insertionSort"
        [ testCase "insertionSort empty" $
            insertionSort ([] :: [Int]) @?= []
        , testCase "insertionSort singleton" $
            insertionSort [1 :: Int] @?= [1]
        , testCase "insertionSort already sorted" $
            insertionSort [1, 2, 3, 4 :: Int] @?= [1, 2, 3, 4]
        , testCase "insertionSort reverse sorted" $
            insertionSort [4, 3, 2, 1 :: Int] @?= [1, 2, 3, 4]
        , testCase "insertionSort mixed" $
            insertionSort [3, 1, 4, 1, 5 :: Int] @?= [1, 1, 3, 4, 5]
        , testProperty "insertionSort result is sorted" $ \(xs :: [Int]) ->
            let s = insertionSort xs
             in and (zipWith (<=) s (tail s)) || length s <= 1
        , testProperty "insertionSort preserves length" $ \(xs :: [Int]) ->
            length (insertionSort xs) == length xs
        ]
    , testGroup
        "mergeSort"
        [ testCase "mergeSort empty" $
            mergeSort ([] :: [Int]) @?= []
        , testCase "mergeSort singleton" $
            mergeSort [1 :: Int] @?= [1]
        , testCase "mergeSort already sorted" $
            mergeSort [1, 2, 3, 4 :: Int] @?= [1, 2, 3, 4]
        , testCase "mergeSort reverse sorted" $
            mergeSort [4, 3, 2, 1 :: Int] @?= [1, 2, 3, 4]
        , testCase "mergeSort mixed" $
            mergeSort [3, 1, 4, 1, 5 :: Int] @?= [1, 1, 3, 4, 5]
        , testProperty "mergeSort result is sorted" $ \(xs :: [Int]) ->
            let s = mergeSort xs
             in and (zipWith (<=) s (tail s)) || length s <= 1
        , testProperty "mergeSort preserves length" $ \(xs :: [Int]) ->
            length (mergeSort xs) == length xs
        , testProperty "insertionSort == mergeSort" $ \(xs :: [Int]) ->
            insertionSort xs == mergeSort xs
        ]
    , testGroup
        "facList"
        [ testCase "facList 0 == 1" $
            facList 0 @?= 1
        , testCase "facList 1 == 1" $
            facList 1 @?= 1
        , testCase "facList 5 == 120" $
            facList 5 @?= 120
        , testCase "facList 10 == 3628800" $
            facList 10 @?= 3628800
        ]
    , testGroup
        "countdown"
        [ testCase "countdown 0 == []" $
            countdown 0 @?= []
        , testCase "countdown 3 == [3,2,1]" $
            countdown 3 @?= [3, 2, 1]
        , testProperty "lenList (countdown n) == n" $ \(NonNegative (n :: Integer)) ->
            n < 1000 ==> lenList (countdown n) == fromIntegral n
        , testProperty "countdown is strictly decreasing" $ \(NonNegative (n :: Integer)) ->
            n < 100 ==> countdown n == [n, n - 1 .. 1]
        ]
    , testGroup
        "prefixes"
        [ testCase "prefixes empty" $
            prefixes ([] :: [Int]) @?= [[]]
        , testCase "prefixes [1,2,3]" $
            prefixes [1, 2, 3 :: Int] @?= [[], [1], [1, 2], [1, 2, 3]]
        , testProperty "first prefix is always []" $ \(xs :: [Int]) ->
            head (prefixes xs) == []
        , testProperty "last prefix is the list itself" $ \(xs :: [Int]) ->
            last (prefixes xs) == xs
        , testProperty "number of prefixes == length + 1" $ \(xs :: [Int]) ->
            length (prefixes xs) == length xs + 1
        , testProperty "prefixes are all prefixes of the original" $ \(xs :: [Int]) ->
            all (\p -> take (length p) xs == p) (prefixes xs)
        ]
    , testGroup
        "suffixes"
        [ testCase "suffixes empty" $
            suffixes ([] :: [Int]) @?= []
        , testCase "suffixes [1,2,3]" $
            suffixes [1, 2, 3 :: Int] @?= [[1, 2, 3], [2, 3], [3]]
        , testProperty "first suffix is the list itself" $ \(NonEmpty (xs :: [Int])) ->
            head (suffixes xs) == xs
        , testProperty "number of suffixes == length" $ \(xs :: [Int]) ->
            length (suffixes xs) == length xs
        , testProperty "suffixes are all suffixes of the original" $ \(xs :: [Int]) ->
            all (\s -> drop (length xs - length s) xs == s) (suffixes xs)
        ]
    , testGroup
        "diff"
        [ testCase "diff removes elements" $
            diff [1, 2, 3, 4 :: Int] [2, 4] @?= [1, 3]
        , testCase "diff with empty second" $
            diff [1, 2, 3 :: Int] [] @?= [1, 2, 3]
        , testCase "diff with empty first" $
            diff ([] :: [Int]) [1, 2] @?= []
        , testCase "diff with identical lists" $
            diff [1, 2, 3 :: Int] [1, 2, 3] @?= []
        , testProperty "diff xs [] == xs" $ \(xs :: [Int]) ->
            diff xs [] == xs
        , testProperty "diff xs xs == []" $ \(xs :: [Int]) ->
            diff xs xs == []
        ]
    , testGroup
        "chunksOf"
        [ testCase "chunksOf 2 [1..4]" $
            chunksOf 2 [1, 2, 3, 4 :: Int] @?= [[1, 2], [3, 4]]
        , testCase "chunksOf 3 [1..7] (uneven)" $
            chunksOf 3 [1 .. 7 :: Int] @?= [[1, 2, 3], [4, 5, 6], [7]]
        , testCase "chunksOf n empty" $
            chunksOf 3 ([] :: [Int]) @?= []
        , testProperty "concat . chunksOf n == id" $ \(Positive n) (xs :: [Int]) ->
            concat (chunksOf n xs) == xs
        , testProperty "all chunks have size <= n" $ \(Positive n) (xs :: [Int]) ->
            all (\c -> length c <= n) (chunksOf n xs)
        ]
    , testGroup
        "noRepeats"
        [ testCase "noRepeats empty" $
            noRepeats ([] :: [Int]) @?= True
        , testCase "noRepeats singleton" $
            noRepeats [1 :: Int] @?= True
        , testCase "noRepeats with no duplicates" $
            noRepeats [1, 2, 3, 4 :: Int] @?= True
        , testCase "noRepeats with duplicates" $
            noRepeats [1, 2, 2, 3 :: Int] @?= False
        , testProperty "noRepeats == (nub == id)" $ \(xs :: [Int]) ->
            noRepeats xs
              == (length (foldr (\x acc -> if x `elem` acc then acc else x : acc) [] xs) == length xs)
        ]
    , testGroup
        "plusplus"
        [ testCase "plusplus two lists" $
            plusplus [1, 2 :: Int] [3, 4] @?= [1, 2, 3, 4]
        , testCase "plusplus empty left" $
            plusplus [] [1, 2 :: Int] @?= [1, 2]
        , testCase "plusplus empty right" $
            plusplus [1, 2 :: Int] [] @?= [1, 2]
        , testProperty "plusplus == (++)" $ \(xs :: [Int]) (ys :: [Int]) ->
            plusplus xs ys == xs ++ ys
        ]
    , testGroup
        "join & sep"
        [ testCase "join produces tagged list" $
            join ([1, 2 :: Int], [True, False]) @?= [Left 1, Left 2, Right True, Right False]
        , testCase "sep splits correctly" $
            sep [Left 1, Right True, Left 2, Right False :: Either Int Bool] @?= ([1, 2], [True, False])
        , testCase "sep . join == id" $
            sep (join ([1, 2 :: Int], [True, False])) @?= ([1, 2], [True, False])
        , testProperty "sep . join == id (property)" $ \(xs :: [Int]) (ys :: [Bool]) ->
            sep (join (xs, ys)) == (xs, ys)
        ]
    ]
