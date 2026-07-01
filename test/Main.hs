module Main (main) where

import qualified AlgebraSpec
import qualified MaybeSpec
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "Endless Tests"
      [ AlgebraSpec.tests,
        MaybeSpec.tests
      ]
