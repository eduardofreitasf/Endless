module Main (main) where

import qualified AlgebraSpec
import qualified ListSpec
import qualified MaybeSpec
import qualified NatSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Endless Tests"
      [ AlgebraSpec.tests,
        MaybeSpec.tests,
        NatSpec.tests,
        ListSpec.tests
      ]
