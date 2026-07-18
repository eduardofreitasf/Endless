{-# LANGUAGE ScopedTypeVariables #-}

module MaybeSpec (tests) where

import Maybe
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Maybe Module Tests"
    [ testGroup
        "Base Functor & Isomorphisms"
        [ testCase "inMaybe Left" $
            inMaybe (Left ()) @?= (Nothing :: Maybe Int)
        , testCase "inMaybe Right" $
            inMaybe (Right 5) @?= Just 5
        , testCase "outMaybe Nothing" $
            outMaybe (Nothing :: Maybe Int) @?= Left ()
        , testCase "outMaybe Just" $
            outMaybe (Just 5) @?= Right 5
        , testProperty "inMaybe . outMaybe == id" $ \(x :: Maybe Int) ->
            inMaybe (outMaybe x) == x
        , testProperty "outMaybe . inMaybe == id" $ \(x :: Either () Int) ->
            outMaybe (inMaybe x) == x
        ]
    , testGroup
        "Recursion Schemes (Ana, Cata, Hylo)"
        [ testProperty "anaMaybe is inMaybe . g" $ \(x :: Maybe Int) ->
            let g = outMaybe :: Maybe Int -> Either () Int
             in anaMaybe g x == (inMaybe (g x) :: Maybe Int)
        , testProperty "cataMaybe is g . outMaybe" $ \(x :: Maybe Int) ->
            let g = inMaybe :: Either () Int -> Maybe Int
             in cataMaybe g x == g (outMaybe x)
        , testProperty "hyloMaybe is f . g" $ \(x :: Maybe Int) ->
            let f = inMaybe :: Either () Int -> Maybe Int
                g = outMaybe :: Maybe Int -> Either () Int
             in hyloMaybe f g x == f (g x)
        ]
    , testGroup
        "mapMaybe"
        [ testCase "mapMaybe Nothing" $
            mapMaybe (+ 1) Nothing @?= (Nothing :: Maybe Int)
        , testCase "mapMaybe Just" $
            mapMaybe (+ 1) (Just 3) @?= Just 4
        , testProperty "mapMaybe id == id" $ \(x :: Maybe Int) ->
            mapMaybe id x == x
        , testProperty "mapMaybe (f . g) == mapMaybe f . mapMaybe g" $
            \(Fn f :: Fun Int Int) (Fn g :: Fun Int Int) (x :: Maybe Int) ->
              mapMaybe (f . g) x == mapMaybe f (mapMaybe g x)
        ]
    , testGroup
        "filterMaybe"
        [ testCase "filterMaybe keeps value (even 4)" $
            filterMaybe even (Just 4) @?= Just 4
        , testCase "filterMaybe drops value (even 3)" $
            filterMaybe even (Just 3) @?= Nothing
        , testCase "filterMaybe Nothing" $
            filterMaybe even Nothing @?= Nothing
        , testProperty "filterMaybe consistency" $ \(p :: Fun Int Bool) (x :: Maybe Int) ->
            filterMaybe (applyFun p) x == (if isJust x && applyFun p (fromMaybe 0 x) then x else Nothing)
        ]
    , testGroup
        "fromMaybe"
        [ testCase "fromMaybe Nothing fallback" $
            fromMaybe 7 Nothing @?= 7
        , testCase "fromMaybe Just value" $
            fromMaybe 7 (Just 5) @?= 5
        , testProperty "fromMaybe fallback property" $ \(d :: Int) (x :: Maybe Int) ->
            fromMaybe d x == case x of
              Nothing -> d
              Just v -> v
        ]
    , testGroup
        "maybeToList"
        [ testCase "maybeToList Nothing" $
            maybeToList (Nothing :: Maybe Int) @?= []
        , testCase "maybeToList Just" $
            maybeToList (Just 1) @?= [1]
        , testProperty "maybeToList length is <= 1" $ \(x :: Maybe Int) ->
            length (maybeToList x) <= 1
        , testProperty "maybeToList identity property" $ \(x :: Maybe Int) ->
            maybeToList x == case x of
              Nothing -> []
              Just v -> [v]
        ]
    , testGroup
        "isJust"
        [ testCase "isJust Nothing" $
            isJust (Nothing :: Maybe Int) @?= False
        , testCase "isJust Just" $
            isJust (Just 1) @?= True
        , testProperty "isJust consistency with null . maybeToList" $ \(x :: Maybe Int) ->
            isJust x == not (null (maybeToList x))
        ]
    , testGroup
        "joinMaybe"
        [ testCase "joinMaybe Nothing" $
            joinMaybe (Nothing :: Maybe (Maybe Int)) @?= Nothing
        , testCase "joinMaybe Just Nothing" $
            joinMaybe (Just Nothing :: Maybe (Maybe Int)) @?= Nothing
        , testCase "joinMaybe Just Just" $
            joinMaybe (Just (Just 2)) @?= Just 2
        , testProperty "joinMaybe property" $ \(x :: Maybe (Maybe Int)) ->
            joinMaybe x == case x of
              Nothing -> Nothing
              Just v -> v
        ]
    , testGroup
        "guardMaybe"
        [ testCase "guardMaybe odd 3 (False)" $
            guardMaybe even 3 @?= Nothing
        , testCase "guardMaybe even 4 (True)" $
            guardMaybe even 4 @?= Just 4
        , testProperty "guardMaybe property" $ \(p :: Fun Int Bool) (x :: Int) ->
            guardMaybe (applyFun p) x == (if applyFun p x then Just x else Nothing)
        ]
    , testGroup
        "totMaybe"
        [ testCase "totMaybe applies function (True)" $
            totMaybe (+ 1) odd 3 @?= Just 4
        , testCase "totMaybe returns Nothing (False)" $
            totMaybe (+ 1) even 3 @?= Nothing
        , testProperty "totMaybe property" $ \(f :: Fun Int Int) (p :: Fun Int Bool) (x :: Int) ->
            totMaybe (applyFun f) (applyFun p) x == (if applyFun p x then Just (applyFun f x) else Nothing)
        ]
    ]
