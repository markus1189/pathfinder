{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PathFinder.Core ( Path
                       , pathLength
                       , pathCoords

                       , pathFinderSearch
                       ) where

import Control.Lens.Operators
import Control.Lens (view, use)
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

data Path l c = Path { _pathLength :: !l
                     , _pathCoords :: [c]
                     } deriving (Show,Eq)
makeLenses ''Path

type PredecessorMap coord cost = Map.Map coord (cost,Maybe coord)

data PathFinderState coord cost score =
    PathFinderState { _closed :: Set.Set coord
                    , _open :: PSQ.PSQ coord score
                    , _seen :: PredecessorMap coord cost
                    }
makeLenses ''PathFinderState


instance (Ord coord, Ord score) => Default (PathFinderState coord cost score) where
  def = PathFinderState def PSQ.empty def

data PathFinderConfig coord cost score =
    PathFinderConfig { _canBeWalked :: coord -> Bool
                     , _heuristicScore :: coord -> score
                     , _stepCost :: coord -> coord -> cost
                     , _neighbors :: coord -> [coord]
                     , _isGoal :: coord -> Bool
                     }
makeLenses ''PathFinderConfig

newtype PathFinder coord cost score a =
    PathFinder (ReaderT
                (PathFinderConfig coord cost score)
                (StateT (PathFinderState coord score cost) Identity) a)
    deriving ( Functor, Applicative, Monad
             , MonadState (PathFinderState coord score cost)
             , MonadReader (PathFinderConfig coord cost score))

runPathFinder :: PathFinderConfig coord Double Double ->
                 PathFinderState coord Double Double ->
                 PathFinder coord Double Double a ->
                 (a,PathFinderState coord Double Double)
runPathFinder c st (PathFinder a) = runIdentity $ runStateT (runReaderT a c) st

findPath :: Ord coord =>
            coord
         -> PathFinder coord Double Double (Maybe (Path Double coord))
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

visitAndExpand :: ( Ord coord
                  , Applicative m
                  , MonadState (PathFinderState coord Double Double) m
                  , MonadReader (PathFinderConfig coord Double Double) m
                  ) => coord -> m ()
visitAndExpand c = visit c >> expand c >>= analyzeNbs c

reconstructPath :: forall coord. Ord coord =>
                   coord
                -> PredecessorMap coord Double
                -> Maybe (Path Double coord)
reconstructPath finish pmap = do
  (totalCost,predec) <- Map.lookup finish pmap
  case predec of
    Nothing -> return $ Path 0 []
    Just _ -> return $ Path totalCost (reverse $ go finish)
  where
    go :: Ord coord => coord -> [coord]
    go current = case Map.lookup current pmap of
      Nothing -> []
      Just (_, Just predec) -> current : go predec
      Just (_, Nothing) -> [current]

nodesLeftToExpand :: ( Functor m
                     , MonadState (PathFinderState coord Double Double) m) => m Int
nodesLeftToExpand = PSQ.size <$> use open

expand :: ( Applicative m
          , MonadReader (PathFinderConfig coord cost score) m
          ) => coord -> m [coord]
expand coord = filter <$> view canBeWalked <*> (view neighbors <*> pure coord)

analyzeNb :: ( Ord coord
             , Applicative m
             , MonadReader (PathFinderConfig coord Double Double) m
             , MonadState (PathFinderState coord Double Double) m
             ) => coord -> coord -> m ()
analyzeNb predecessor nb = do
  alreadySeen <- alreadyVisited nb
  costSoFar <- costFor predecessor
  estimation <- view stepCost <*> pure predecessor <*> pure nb
  let newCost = (+) <$> costSoFar <*> pure estimation
  mayUpdateCost newCost nb predecessor
  unless alreadySeen $ do
      heuristicValue <- view heuristicScore <*> pure nb
      when (isJust newCost) $
        open %= insertIfNotPresent nb (fromJust newCost + heuristicValue)

analyzeNbs :: ( Ord coord
              , Applicative m
              , MonadReader (PathFinderConfig coord Double Double) m
              , MonadState (PathFinderState coord Double Double) m
              ) => coord -> [coord] -> m ()
analyzeNbs predecessor = mapM_ (analyzeNb predecessor)

costFor :: ( Ord coord
           , Functor m
           , MonadState (PathFinderState coord cost Double) m
           ) => coord -> m (Maybe cost)
costFor c = (fmap . fmap) fst $ Map.lookup c <$> use seen

mayUpdateCost :: ( Ord coord
                 , Ord cost
                 , Functor m
                 , MonadState (PathFinderState coord cost Double) m
                 ) => Maybe cost -> coord -> coord -> m ()
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

visit :: Ord coord => MonadState (PathFinderState coord Double Double) m => coord -> m ()
visit c = closed %= Set.insert c

alreadyVisited :: ( Ord coord
                  , Functor m
                  , MonadState (PathFinderState coord Double Double) m) => coord -> m Bool
alreadyVisited c = Set.member c <$> use closed

pathFinderSearch :: forall coord. Ord coord =>
                    (coord -> [coord])
                 -> (coord -> Bool)
                 -> (coord -> Double)
                 -> (coord -> coord -> Double)
                 -> coord
                 -> (coord -> Bool)
                 -> (Maybe (Path Double coord), PathFinderState coord Double Double)
pathFinderSearch nbs walkable heuristic step start isGoalP =
  runPathFinder config initState $ findPath start
  where
    config = PathFinderConfig walkable heuristic step nbs isGoalP
    initState :: (PathFinderState coord Double Double)
    initState = def & seen %~ Map.insert start (0,Nothing)
                    & open %~ PSQ.insert start 0
