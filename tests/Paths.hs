module Paths (tests) where

import Test.Tasty
import Test.Tasty.Hspec

tests :: TestTree
tests = testGroup "tests"
     [ testCase "smoke" smokeSpec
     ]

smokeSpec :: Spec
smokeSpec = describe "smoke test" $ it "runs" $ (1::Int) `shouldBe` 1
