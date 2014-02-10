{-# LANGUAGE TemplateHaskell #-}
module PathFinder.Types ( PathFinderConfig
                        , undefinedConfig
                        , canBeWalked
                        , heuristicScore
                        , stepCost
                        , neighbors
                        , isGoal
                        , combineCostScore

                        , Path (Path)
                        , pathCost
                        , pathCoords
                        )where

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

undefinedConfig :: PathFinderConfig coord cost score
undefinedConfig = PathFinderConfig { _canBeWalked = errMsg "canBeWalked"
                                   , _heuristicScore = errMsg "heuristicScore"
                                   , _stepCost = errMsg "stepCost"
                                   , _neighbors = errMsg "neighbors"
                                   , _isGoal = errMsg "isGoal"
                                   , _combineCostScore = errMsg "combineCostScore"
                                   }
  where errMsg s = error $ s ++ " undefined in PathFinderConfig"

data Path l c = Path { _pathCost :: !l
                     , _pathCoords :: [c]
                     } deriving (Show,Eq)
makeLenses ''Path
