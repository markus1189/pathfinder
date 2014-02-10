module Paths (tests) where

import           Data.Monoid
import           PathFinder.Core
import           PathFinder.Types
import           Control.Lens (preview, _Just)
import           Test.Tasty
import           Test.Tasty.Hspec
import qualified Data.Map as Map

tests :: TestTree
tests = testGroup "tests"
     [ testCase "finding a path" spec
     ]

spec :: Spec
spec = describe "Path finding functionality" $
  it "should be able to reconstruct the path, given a predecessor map" $ do
    let zero = 0 :: Int
        pmap = Map.fromList [ ((zero,zero), (Sum zero, Nothing))
                            , ((0,1), (Sum 0, Just (0,0)))
                            , ((0,2), (Sum 0, Just (0,1)))
                            ]
    preview (_Just . pathCoords) (reconstructPath (0,2) pmap) `shouldBe`
         Just [(0,0), (0,1), (0,2)]
  -- it "should not expand to disallowed coords" $ do
  -- it "should add the cell to closed set after visiting it" $ do

  -- context "analyzing neighbors" $ do
  --   context "if the neighbor was unknown" $ do
  --     it "should insert an unknown neighbor into the predecessor map" $ do
  --     it "should add new neighbors to the open set" $ do
  --   it "should update the predecessor map if the new path is shorter" $ do
  --   it "should do nothing if the path to the predecessor is unknown" $ do
  -- context "finding a path" $ do
  --   it "should return an empty path if start == goal" $ do
  --   it "should find a straight path" $ do
  --   it "should find a path around an obstacle" $ do
  --   it "should terminate if path is blocked" $ do
  --   it "should find a path around a more complicated obstacle" $ do
