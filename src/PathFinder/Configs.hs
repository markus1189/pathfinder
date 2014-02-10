module PathFinder.Configs ( searchIn2d
                          , searchInMatrix
                          ) where

import Control.Lens.Operators
import Control.Lens (_2, view)
import Control.Monad (guard)
import Data.List (genericIndex)
import Data.Maybe (fromJust, isJust)
import Data.Monoid ((<>), Sum(Sum))
import Linear.Metric (distance, Metric)
import Linear.V2 (_x, _y, R2)

import PathFinder.Types

searchIn2d :: (Eq a, Eq (f a), Floating a, Metric f, R2 f) =>
     f a -> PathFinderConfig (f a) (Sum a) (Sum a)
searchIn2d end = PathFinderConfig { _canBeWalked = const True
                                  , _heuristicScore = Sum . (`distance` end)
                                  , _stepCost = \c1 c2 -> Sum $ distance c1 c2
                                  , _neighbors = neighbors2d
                                  , _isGoal = (== end)
                                  , _combineCostScore = (<>)
                                  }

neighbors2d :: (Eq d, Num d, R2 v) => v d -> [v d]
neighbors2d coord = do
  dx <- [-1,0,1]
  dy <- [-1,0,1]
  guard $ (dx,dy) /= (0,0)
  return $ coord & _x +~ dx & _y +~ dy

searchInMatrix :: Integral a =>
                  a -> [[Maybe a]] -> PathFinderConfig a (Sum a) (Sum a)
searchInMatrix end matrix =
  PathFinderConfig { _canBeWalked = const True
                   , _heuristicScore = const (Sum 1)
                   , _stepCost = \x y -> Sum . fromJust $
                                 (matrix `genericIndex` y `genericIndex` x)
                   , _neighbors = matrixNeighbors matrix
                   , _isGoal = (== end)
                   , _combineCostScore = (<>)
                   }

matrixNeighbors :: Integral i => [[Maybe i]] -> i -> [i]
matrixNeighbors m i = map fst . filter (isJust . view _2 ) $ zip [0..] row
  where row = m `genericIndex` i
