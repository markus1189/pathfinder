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

                     , Path
                     , pathLength
                     , pathCoords

                     , Coord
                     ) where

import Control.Lens (view, (%=), use, (.=))
import Control.Lens.TH
import Control.Applicative (Applicative, (<*>),(<$>),pure)
import Control.Monad(when, unless)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT, runStateT)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader.Class
import Control.Monad.State.Class

import Data.Default
import Data.Maybe (isJust,fromJust)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.PSQueue as PSQ

import Linear.V3 (V3(..))

type HeuristicScore = Double
type WayCost = Double

type Coord = V3 Double

data Path l c = Path { _pathLength :: !l
                     , _pathCoords :: [c]
                     } deriving (Show,Eq)
makeLenses ''Path

type PredecessorMap = Map.Map Coord (WayCost,Maybe Coord)

data PathFinderState c = PathFinderState { _closed :: Set.Set c
                                         , _open :: PSQ.PSQ c HeuristicScore
                                         , _seen :: PredecessorMap
                                         }
makeLenses ''PathFinderState


instance Ord c => Default (PathFinderState c) where
  def = PathFinderState def PSQ.empty def

data PathFinderConfig c = PathFinderConfig { _canBeWalked :: c -> Bool
                                           , _heuristicCost :: c -> HeuristicScore
                                           , _stepCost :: c -> c -> WayCost
                                           , _neighbors :: c -> [c]
                                           , _isGoal :: c -> Bool
                                           }
makeLenses ''PathFinderConfig

newtype PathFinder a = PathFinder (ReaderT (PathFinderConfig Coord) (
                                      StateT (PathFinderState Coord) Identity) a)
    deriving ( Functor, Applicative, Monad
             , MonadState (PathFinderState Coord), MonadReader (PathFinderConfig Coord))

runPathFinder :: PathFinderConfig Coord ->
                 PathFinderState Coord ->
                 PathFinder a ->
                 (a,PathFinderState Coord)
runPathFinder c st (PathFinder a) = runIdentity $ runStateT (runReaderT a c) st

execPathFinder :: PathFinderConfig Coord ->
                  PathFinderState Coord ->
                  PathFinder a ->
                  PathFinderState Coord
execPathFinder c st a = snd $ runPathFinder c st a

evalPathFinder :: PathFinderConfig Coord ->
                  PathFinderState Coord ->
                  PathFinder a ->
                  a
evalPathFinder c st a = fst $ runPathFinder c st a

findPath :: Coord -> PathFinder (Maybe (Path Double Coord))
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
                  , MonadState (PathFinderState Coord) m
                  , MonadReader (PathFinderConfig Coord) m
                  ) => Coord -> m ()
visitAndExpand c = visit c >> expand c >>= analyzeNbs c

reconstructPath :: Coord -> PredecessorMap -> Maybe (Path Double Coord)
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

nodesLeftToExpand :: (Functor m, MonadState (PathFinderState Coord) m) => m Int
nodesLeftToExpand = PSQ.size <$> use open

expand :: ( Applicative m
          , MonadReader (PathFinderConfig Coord) m
          ) => Coord -> m [Coord]
expand coord = filter <$> view canBeWalked <*> (view neighbors <*> pure coord)

analyzeNb :: ( Applicative m
             , MonadReader (PathFinderConfig Coord) m
             , MonadState (PathFinderState Coord) m
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
              , MonadReader (PathFinderConfig Coord) m
              , MonadState (PathFinderState Coord) m
              ) => Coord -> [Coord] -> m ()
analyzeNbs predecessor = mapM_ (analyzeNb predecessor)

costFor :: ( Functor m
           , MonadState (PathFinderState Coord) m
           ) => Coord -> m (Maybe WayCost)
costFor c = (fmap . fmap) fst $ Map.lookup c <$> use seen

mayUpdateCost :: (Functor m, MonadState (PathFinderState Coord) m) =>
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

visit :: MonadState (PathFinderState Coord) m => Coord -> m ()
visit c = closed %= Set.insert c

alreadyVisited :: (Functor m, MonadState (PathFinderState Coord) m) => Coord -> m Bool
alreadyVisited c = Set.member c <$> use closed
