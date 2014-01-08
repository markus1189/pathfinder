module Path ( searchPath
            , existsPath
            , pathCost
            , defaultPath
            , findArea

            , Path
            , pathLength
            , pathCoords
            ) where

import Control.Lens ((&), (%~), view)
import Control.Monad (guard)

import Data.Default
import Data.Maybe (isJust)
import Data.Monoid (Monoid, mempty, mappend)
import Data.Function (on)

import Linear.V3 (V3(V3))

import qualified Data.Map as Map
import qualified Data.PSQueue as PSQ
import qualified Linear.Metric as LM

import Path.Internal

type Coord = V3 Double

searchPath :: (Coord -> Bool)            -- ^ Check if coord is allowed
           -> (Coord -> Double)          -- ^ Heuristic value for the coord
           -> (Coord -> Coord -> Double) -- ^ Cost to go from coord to coord
           -> Coord                      -- ^ The start coord
           -> (Coord -> Bool)            -- ^ Check if current coord is the goal
           -> Maybe (Path Double Coord)  -- ^ Just the path from start to goal or Nothing
searchPath w h step start isGoal = fst $ pathFinderSearch w h step start isGoal

existsPath :: (Coord -> Bool)            -- ^ Check if coord is allowed
           -> (Coord -> Double)          -- ^ Heuristic value for the coord
           -> (Coord -> Coord -> Double) -- ^ Cost to go from coord to coord
           -> Coord                      -- ^ The start coord
           -> Coord                      -- ^ The goal coord
           -> Bool                       -- ^ Whether there exists a path from start to goal
existsPath w h step start goal = isJust $ searchPath w h step start (== goal)

pathCost :: (Coord -> Bool)            -- ^ Check if coord is allowed
         -> (Coord -> Double)          -- ^ Heuristic value for the coord
         -> (Coord -> Coord -> Double) -- ^ Cost to go from coord to coord
         -> Coord                      -- ^ The start coord
         -> Coord                      -- ^ The goal coord
         -> Maybe Double               -- ^ Cost of a path from start to goal
pathCost w h step start goal = fmap (view pathLength) (searchPath w h step start (== goal))

-- | Searches a path from 'start' to 'goal', without walking cells
-- that are not allowed according to the given predicate. The
-- heuristic is assumed to be the euclidean distance.
-- The way cost from a Coord to any of its neighbors is assumed to be always 1
defaultPath :: (Coord -> Bool) -> Coord -> Coord -> Maybe (Path Double Coord)
defaultPath walkable start goal =
  searchPath walkable (LM.distance goal) (const . const $ 1) start (== goal)

-- | Finds a path to a nearest coordinate that is part of the
-- specified area.
findArea :: (Coord -> Bool) -> Coord -> [Coord] -> Maybe (Path Double Coord)
findArea _ _ [] = Nothing
findArea walkable start goals =
  searchPath walkable heuristic (const . const $ 1) start (`elem` goals)
  where
    heuristic :: Coord -> Double
    heuristic coord = minimum $ map (LM.distance coord) goals

-- private helper
pathFinderSearch :: (Coord -> Bool)
                 -> (Coord -> Double)
                 -> (Coord -> Coord -> Double)
                 -> Coord
                 -> (Coord -> Bool)
                 -> (Maybe (Path Double Coord), PathFinderState Coord)
pathFinderSearch walkable heuristic step start isGoal =
  runPathFinder config initState $ findPath start
  where
    config = PathFinderConfig walkable heuristic step neighbors2d isGoal
    initState :: (PathFinderState Coord)
    initState = def & seen %~ Map.insert start (0,Nothing)
                    & open %~ PSQ.insert start 0

newtype SumCoord = SumCoord { getSumCoord :: Coord } deriving Show

instance Monoid SumCoord where
  mempty = SumCoord $ V3 0 0 0
  mappend (SumCoord (V3 x1 y1 z1)) (SumCoord (V3 x2 y2 z2)) =
    SumCoord $ V3 (x1+x2) (y1+y2) (z1+z2)

infixl 6 |+|
(|+|) :: Coord -> Coord -> Coord
c1 |+| c2 = getSumCoord $ on mappend SumCoord c1 c2

from2d :: (Double,Double) -> Coord
from2d (x,y) = V3 x y 0

directions2d :: [Coord]
directions2d = do
  x <- [-1, 0, 1]
  y <- [-1, 0, 1]
  guard $ (x,y) /= (0,0)
  return . from2d $ (x,y)

neighbors2d :: Coord -> [Coord]
neighbors2d c = [c |+| dir | dir <- directions2d]
