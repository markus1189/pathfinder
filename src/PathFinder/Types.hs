{-# LANGUAGE TemplateHaskell #-}
module PathFinder.Types ( PathFinderConfig (..)
                        , pathFinderConfig
                        , canBeWalked
                        , heuristicScore
                        , stepCost
                        , neighbors
                        , isGoal
                        , combineCostScore

                        , Path (Path)
                        , pathCost
                        , pathCoords
                        ) where

import Control.Lens.TH

data PathFinderConfig coord cost score =
    PathFinderConfig { _canBeWalked :: coord -> Bool
                     , _heuristicScore :: coord -> score
                     , _stepCost :: coord -> coord -> cost
                     , _neighbors :: coord -> [coord]
                     , _isGoal :: coord -> Bool
                     , _combineCostScore :: cost -> score -> cost
                     }
makeLenses ''PathFinderConfig

pathFinderConfig :: (coord -> Bool)
                 -> (coord -> score)
                 -> (coord -> coord -> cost)
                 -> (coord -> [coord])
                 -> (coord -> Bool)
                 -> (cost -> score -> cost)
                 -> PathFinderConfig coord cost score
pathFinderConfig = PathFinderConfig

data Path l c = Path { _pathCost :: !l
                     , _pathCoords :: [c]
                     } deriving (Show,Eq)
makeLenses ''Path
