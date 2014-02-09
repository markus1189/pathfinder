module Main where

import Test.Tasty
import qualified Paths

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Paths.tests]
