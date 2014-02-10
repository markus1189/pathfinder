module PathFinder.Interface ( search2d
                            , search2d'
                            , searchGraphMatrix
                            , graph
                            ) where

import PathFinder.Core ( pathFinderSearch
                       , PathFinderState
                       )
import PathFinder.Types
import PathFinder.Configs
import Control.Lens.Operators
import Control.Lens (_Just, _1)
import Linear.V2 (V2)
import Data.Monoid ( Sum(getSum) )

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
    where cfg = searchIn2d end & canBeWalked %~ \prevBlock x -> prevBlock x || blocked x

searchGraphMatrix :: Integral i => i -> i -> [[Maybe i]] -> Maybe (Path i i)
searchGraphMatrix start end matrix =
    fst (pathFinderSearch cfg start) & _Just . pathCost %~ getSum
  where cfg = searchInMatrix end matrix

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
