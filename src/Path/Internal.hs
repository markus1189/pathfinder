{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, FlexibleContexts #-}
module Path.Internal ( findPath
                     , expand
                     , visit
                     , analyzeNbs
                     , reconstructPath

                     , PathFinderState (PathFinderState)
                     , closed
                     , open
                     , seen
                     , alreadyVisited

                     , PathFinderConfig (PathFinderConfig)
                     , canBeWalked
                     , heuristicCost

                     , PathFinder
                     , runPathFinder
                     , evalPathFinder
                     , execPathFinder

                     , Path (Path)
                     , pathLength
                     , pathCoords

                     , Coord (Coord)
                     , distance
                     , neighbors2d

                     , cx
                     , cy
                     , cz
                     ) where

import Control.Lens (view, (%=), use, (.=))
import Control.Lens.TH
import Control.DeepSeq (NFData (rnf))
import Control.Applicative (Applicative, (<*>),(<$>),pure)
import Control.Monad(when, unless, guard)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT, runStateT)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader.Class
import Control.Monad.State.Class

import Data.Default
import Data.Maybe (isJust,fromJust)
import Data.Monoid (Monoid, mempty, mappend)
import Data.Function (on)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.PSQueue as PSQ

type HeuristicScore = Double
type WayCost = Double

data Coord = Coord { _cx :: Int, _cy :: Int, _cz :: Int } deriving (Eq,Ord,Show)
makeLenses ''Coord

distance :: Coord -> Coord -> Double
distance (Coord x1 y1 z1) (Coord x2 y2 z2) = sqrt $ xSum + ySum + zSum
  where
    xSum = fromIntegral . square $ x1 - x2
    ySum = fromIntegral . square $ y1 - y2
    zSum = fromIntegral . square $ z1 - z2
    square x = x * x

newtype SumCoord = SumCoord { getSumCoord :: Coord } deriving Show

instance Monoid SumCoord where
  mempty = SumCoord $ Coord 0 0 0
  mappend (SumCoord (Coord x1 y1 z1)) (SumCoord (Coord x2 y2 z2)) =
    SumCoord $ Coord (x1+x2) (y1+y2) (z1+z2)

infixl 6 |+|
(|+|) :: Coord -> Coord -> Coord
c1 |+| c2 = getSumCoord $ on mappend SumCoord c1 c2

from2d :: (Int,Int) -> Coord
from2d (x,y) = Coord x y 0

directions2d :: [Coord]
directions2d = do
  x <- [-1, 0, 1]
  y <- [-1, 0, 1]
  guard $ (x,y) /= (0,0)
  return . from2d $ (x,y)

neighbors2d :: Coord -> [Coord]
neighbors2d c = [c |+| dir | dir <- directions2d]

data Path = Path { _pathLength :: !Double
                 , _pathCoords :: [Coord]
                 } deriving (Show,Eq)
makeLenses ''Path

instance NFData Path where
  rnf (Path l cs) = l `seq` cs `seq` ()

type PredecessorMap = Map.Map Coord (WayCost,Maybe Coord)
data PathFinderState = PathFinderState { _closed :: Set.Set Coord
                                       , _open :: PSQ.PSQ Coord HeuristicScore
                                       , _seen :: PredecessorMap
                                       }
makeLenses ''PathFinderState


instance Default PathFinderState where
  def = PathFinderState def PSQ.empty def

data PathFinderConfig = PathFinderConfig { _canBeWalked :: Coord -> Bool
                                         , _heuristicCost :: Coord -> HeuristicScore
                                         , _stepCost :: Coord -> Coord -> WayCost
                                         , _neighbors :: Coord -> [Coord]
                                         , _isGoal :: Coord -> Bool
                                         }
makeLenses ''PathFinderConfig

newtype PathFinder a = PathFinder (ReaderT PathFinderConfig (
                                      StateT PathFinderState Identity) a)
    deriving ( Functor, Applicative, Monad
             , MonadState PathFinderState, MonadReader PathFinderConfig)

runPathFinder :: PathFinderConfig ->
                 PathFinderState ->
                 PathFinder a ->
                 (a,PathFinderState)
runPathFinder c st (PathFinder a) = runIdentity $ runStateT (runReaderT a c) st

execPathFinder :: PathFinderConfig ->
                  PathFinderState ->
                  PathFinder a ->
                  PathFinderState
execPathFinder c st a = snd $ runPathFinder c st a

evalPathFinder :: PathFinderConfig ->
                  PathFinderState ->
                  PathFinder a ->
                  a
evalPathFinder c st a = fst $ runPathFinder c st a

findPath :: Coord -> PathFinder (Maybe Path)
findPath current = do
  goalReached <- view isGoal <*> pure current
  if goalReached
    then reconstructPath current <$> use seen
    else do
      visitAndExpand current
      nodesLeft <- nodesLeftToExpand
      ifGreaterZero nodesLeft $ do
        Just (nextMin PSQ.:-> _, queue) <- PSQ.minView <$> use open
        open .= queue
        findPath nextMin
  where
    ifGreaterZero :: Monad m => Int -> m (Maybe a) -> m (Maybe a)
    ifGreaterZero n action = if n == 0
                               then return Nothing
                               else action

visitAndExpand :: ( Applicative m
                  , MonadState PathFinderState m
                  , MonadReader PathFinderConfig m
                  ) => Coord -> m ()
visitAndExpand c = visit c >> expand c >>= analyzeNbs c

reconstructPath :: Coord -> PredecessorMap -> Maybe Path
reconstructPath finish pmap = do
  (totalCost,predec) <- Map.lookup finish pmap
  case predec of
    Nothing -> return $ Path 0 []
    Just _ -> return $ Path totalCost (reverse $ go finish)
  where
    go :: Coord -> [Coord]
    go current = case Map.lookup current pmap of
      Nothing -> []
      Just (_, Just predec) -> current : go predec
      Just (_, Nothing) -> [current]

nodesLeftToExpand :: (Functor m, MonadState PathFinderState m) => m Int
nodesLeftToExpand = PSQ.size <$> use open

expand :: ( Applicative m
          , MonadReader PathFinderConfig m
          ) => Coord -> m [Coord]
expand coord = filter <$> view canBeWalked <*> (view neighbors <*> pure coord)

analyzeNb :: ( Applicative m
             , MonadReader PathFinderConfig m
             , MonadState PathFinderState m
             ) => Coord -> Coord -> m ()
analyzeNb predecessor nb = do
  alreadySeen <- alreadyVisited nb
  costSoFar <- costFor predecessor
  estimation <- view stepCost <*> pure predecessor <*> pure nb
  let newCost = (+) <$> costSoFar <*> pure estimation
  mayUpdateCost newCost nb predecessor
  unless alreadySeen $ do
      heuristicValue <- view heuristicCost <*> pure nb
      when (isJust newCost) $
        open %= insertIfNotPresent nb (fromJust newCost + heuristicValue)

analyzeNbs :: ( Applicative m
              , MonadReader PathFinderConfig m
              , MonadState PathFinderState m
              ) => Coord -> [Coord] -> m ()
analyzeNbs predecessor = mapM_ (analyzeNb predecessor)

costFor :: ( Functor m
           , MonadState PathFinderState m
           ) => Coord -> m (Maybe WayCost)
costFor c = (fmap . fmap) fst $ Map.lookup c <$> use seen

mayUpdateCost :: (Functor m, MonadState PathFinderState m) =>
              Maybe WayCost -> Coord -> Coord -> m ()
mayUpdateCost Nothing _ _ = return ()
mayUpdateCost (Just cost) target origin = do
  previous <- costFor target
  case previous of
    Just previousCost -> when (cost < previousCost) $
      seen %= Map.insert target (cost, Just origin)
    Nothing -> seen %= Map.insert target (cost, Just origin)

insertIfNotPresent :: ( Ord k
                      , Ord p
                      ) => k -> p -> PSQ.PSQ k p -> PSQ.PSQ k p
insertIfNotPresent key prio queue =
  case PSQ.lookup key queue of
    Just _ -> queue
    Nothing -> PSQ.insert key prio queue

visit :: MonadState PathFinderState m => Coord -> m ()
visit c = closed %= Set.insert c

alreadyVisited :: (Functor m, MonadState PathFinderState m) => Coord -> m Bool
alreadyVisited c = Set.member c <$> use closed
