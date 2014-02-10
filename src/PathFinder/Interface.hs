module PathFinder.Interface ( search2d
                            , search2d'
                            , searchGraphMatrix
                            , graph
                            ) where

import PathFinder.Core ( pathFinderSearch
                       , PathFinderState
                       )
import PathFinder.Types
import Control.Lens.Operators
import Control.Lens (_Just, _1)
import Control.Monad (guard)
import Linear.V2 (R2, V2, _x, _y)
import Linear.Metric (distance)
import Data.Maybe (fromJust, isJust)
import Data.List (genericIndex)
import Data.Monoid ( Sum(Sum, getSum)
                   , (<>))

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
         -> (Maybe (Path d (V2 d)), PathFinderState (V2 d) (Sum d))
search2d start end blocked =
    pathFinderSearch cfg start & _1 . _Just . pathCost %~ getSum
    where cfg = undefinedConfig
                & canBeWalked .~ not . blocked
                & heuristicScore .~ Sum . (`distance` end)
                & stepCost .~ (\c1 c2 -> Sum $ distance c1 c2)
                & neighbors .~ neighbors2d
                & isGoal .~ (== end)
                & combineCostScore .~ (<>)

neighbors2d :: (Eq d, Num d, R2 v) => v d -> [v d]
neighbors2d coord = do
  dx <- [-1,0,1]
  dy <- [-1,0,1]
  guard $ (dx,dy) /= (0,0)
  return $ coord & _x +~ dx & _y +~ dy

searchGraphMatrix :: Integral i => i -> i -> [[Maybe i]] -> Maybe (Path i i)
searchGraphMatrix start end matrix =
    fst (pathFinderSearch cfg start) & _Just . pathCost %~ getSum
  where cfg = undefinedConfig
              & canBeWalked .~ const True
              & heuristicScore .~ const (Sum 1)
              & stepCost .~ (\x y ->
                             Sum $ fromJust (matrix `genericIndex` y `genericIndex` x))
              & neighbors .~ matrixNeighbors matrix
              & isGoal .~ (== end)
              & combineCostScore .~ (<>)

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
