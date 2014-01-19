module PathFinder.Interface ( search2d
                            , search2d'
                            , searchGraphMatrix
                            , graph
                            ) where

import PathFinder.Core (pathFinderSearch, Path, PathFinderConfig(..), PathFinderState)
import Control.Lens.Operators
import Control.Lens (allOf, both)
import Control.Monad (guard)
import Linear.V2 (R2, V2, _x, _y)
import Linear.Metric (distance)
import Data.Maybe (fromMaybe, isJust)
import Data.List (genericIndex)

search2d' :: (Ord d, Floating d) =>
            V2 d
         -> V2 d
         -> (V2 d -> Bool)
         -> Maybe (Path d (V2 d))
search2d' start end blocked = fst $ search2d start end blocked

search2d :: (Ord d, Floating d) =>
            V2 d
         -> V2 d
         -> (V2 d -> Bool)
         -> (Maybe (Path d (V2 d)), PathFinderState (V2 d) d)
search2d start end blocked = pathFinderSearch cfg start
    where cfg = PathFinderConfig { _canBeWalked = not . blocked
                                 , _heuristicScore = (`distance` end)
                                 , _stepCost = distance
                                 , _neighbors = neighbors2d
                                 , _isGoal = (== end)
                                 , _combineCostScore = (+)
                                 }

neighbors2d :: (Eq d, Num d, R2 v) => v d -> [v d]
neighbors2d coord = do
  dx <- [-1,0,1]
  dy <- [-1,0,1]
  guard $ allOf both (/=0) (dx,dy)
  return $ coord & _x +~ dx & _y +~ dy

searchGraphMatrix :: Integral i => i -> i -> [[Maybe i]] -> Maybe (Path i i)
searchGraphMatrix start end matrix = fst $ pathFinderSearch cfg start
    where cfg = PathFinderConfig { _canBeWalked = const True
                                 , _heuristicScore = const 1
                                 , _stepCost = \x y -> fromMaybe undefined
                                               (matrix `genericIndex` y
                                                           `genericIndex` x)
                                 , _neighbors = matrixNeighbors matrix
                                 , _isGoal = (== end)
                                 , _combineCostScore = (+)
                                 }

matrixNeighbors :: Integral i => [[Maybe i]] -> i -> [i]
matrixNeighbors m i = map fst . filter (\(_, may) -> isJust may) $ zip [0..] row
    where row = m `genericIndex` i

graph :: [[Maybe Int]]
graph = [ [Nothing, Just 4, Just 2, Nothing, Just 6, Nothing, Nothing, Nothing]
        , Just 4 : replicate 7 Nothing
        , Just 2 : replicate 7 Nothing
        , replicate 7 Nothing ++ [Just 5]
        , Just 6 : replicate 6 Nothing ++ [Just 5]
        , replicate 6 Nothing ++ [Just 2, Just 9]
        , replicate 5 Nothing ++ [Just 2, Nothing, Nothing]
        , [Nothing, Nothing, Nothing, Just 5, Just 5, Just 9, Nothing, Nothing]
        ]
