module PathFinder.Draw where

import qualified Linear.V2 as LV2

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

import Control.Lens.Operators ((^.))

import PathFinder.Core (seen)
import PathFinder.Types
import PathFinder.Interface (search2d)
import qualified PathFinder.Types as PT

import qualified Data.Map as Map
import qualified Control.Lens as Lens

blocked :: (Num d, Ord d, Enum d) => LV2.V2 d -> Bool
blocked (LV2.V2 x y ) = x < -20
                        || x > 20
                        || y < -20
                        || y > 20
                        || (x,y) `elem` [(3,3), (3,4), (4,3), (2,2), (1,1)]
                        || (x == 3 && y /= 7)
                        || (x > 3 && (y == 0 && x /= 18))
                        || (x `elem` [14..19] && y `elem` [0..(-10)])

drawPath :: LV2.R2 v => PT.Path Double (v Double) -> Diagram B R2
drawPath path = stroke (fromVertices points) # lw 0.1
  where coordinates = path ^. pathCoords
        points = map toPoint coordinates

drawSeen :: LV2.R2 v => Map.Map (v Double) (Sum Double, a) -> Diagram B R2
drawSeen m = decoratePath (fromVertices points) squares
  where pointsWithDistance = Map.toList m & Lens.each . Lens._1 %~ toPoint
        points = map fst pointsWithDistance
        dists = map (getSum . fst . snd) pointsWithDistance
        maxDist = maximum dists
        squares = map (\d -> cl d <> sq) dists
        sq = square 1 # lc black # fc white
        cl d = square 1 # opacity (1 - d/maxDist) # lc black # fc gray

drawStartEnd :: LV2.R2 v => v Double -> v Double -> Diagram B R2
drawStartEnd startCoord endCoord = startDia <> endDia
  where startDia = moveTo (toPoint startCoord) (square 1 # fc green)
        endDia = moveTo (toPoint endCoord) (square 1 # fc red)

toPoint :: LV2.R2 v => v Double -> P2
toPoint p = p2 (x,y)
  where x = p ^. LV2._x
        y = p ^. LV2._y

main :: IO ()
main = mainWith $ pathDia <> startEndDia <> seenDia
  where startCoord = LV2.V2 10 (-10)
        endCoord = LV2.V2 2 (-15)
        (mayPath, state) = search2d startCoord endCoord blocked
        seenDia = drawSeen $ state ^. seen
        pathDia = maybe mempty drawPath mayPath
        startEndDia = drawStartEnd startCoord endCoord
