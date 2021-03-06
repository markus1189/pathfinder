{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PathFinder.Core ( PathFinderState
                       , seen

                       , pathFinderSearch
                       , reconstructPath
                       ) where
import PathFinder.Types

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
import Data.Monoid (mempty, mappend, Monoid)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.PSQueue as PSQ

type PredecessorMap coord cost = Map.Map coord (cost,Maybe coord)

data PathFinderState coord cost =
    PathFinderState { _closed :: Set.Set coord
                    , _open :: PSQ.PSQ coord cost
                    , _seen :: PredecessorMap coord cost
                    }
makeLenses ''PathFinderState

instance (Ord coord, Ord cost) => Default (PathFinderState coord cost) where
  def = PathFinderState def PSQ.empty def

newtype PathFinder coord cost score a =
    PathFinder (ReaderT
                (PathFinderConfig coord cost score)
                (StateT (PathFinderState coord cost) Identity) a)
    deriving ( Functor, Applicative, Monad
             , MonadState (PathFinderState coord cost)
             , MonadReader (PathFinderConfig coord cost score))

runPathFinder :: PathFinderConfig coord cost score ->
                 PathFinderState coord cost ->
                 PathFinder coord cost score a ->
                 (a,PathFinderState coord cost)
runPathFinder c st (PathFinder a) = runIdentity $ runStateT (runReaderT a c) st

findPath :: (Ord cost, Ord coord, Ord score, Monoid cost, Monoid score) =>
            coord
         -> PathFinder coord cost score (Maybe (Path cost coord))
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
                  , Ord cost
                  , Monoid cost
                  , Applicative m
                  , MonadState (PathFinderState coord cost) m
                  , MonadReader (PathFinderConfig coord cost score) m
                  ) => coord -> m ()
visitAndExpand c = visit c >> expand c >>= analyzeNbs c

reconstructPath :: forall coord cost. (Ord coord, Monoid cost) =>
                   coord
                -> PredecessorMap coord cost
                -> Maybe (Path cost coord)
reconstructPath finish pmap = do
  (totalCost,predec) <- Map.lookup finish pmap
  case predec of
    Nothing -> return $ Path mempty []
    Just _ -> return $ Path totalCost (reverse $ go finish)
  where
    go :: Ord coord => coord -> [coord]
    go current = case Map.lookup current pmap of
      Nothing -> []
      Just (_, Nothing) -> [current]
      Just (_, Just predec) -> current : go predec

nodesLeftToExpand :: ( Functor m
                     , MonadState (PathFinderState coord cost) m) => m Int
nodesLeftToExpand = PSQ.size <$> use open

expand :: ( Applicative m
          , MonadReader (PathFinderConfig coord cost score) m
          ) => coord -> m [coord]
expand coord = filter <$> view canBeWalked <*> (view neighbors <*> pure coord)

analyzeNb :: ( Ord coord
             , Ord cost
             , Monoid  cost
             , Applicative m
             , MonadReader (PathFinderConfig coord cost score) m
             , MonadState (PathFinderState coord cost) m
             ) => coord -> coord -> m ()
analyzeNb predecessor nb = do
  alreadySeen <- alreadyVisited nb
  costSoFar <- costFor predecessor
  estimation <- view stepCost <*> pure predecessor <*> pure nb
  let newCost = mappend <$> costSoFar <*> pure estimation
  mayUpdateCost newCost nb predecessor
  unless alreadySeen $ do
      heuristicValue <- view heuristicScore <*> pure nb
      when (isJust newCost) $ do
        plus <- view combineCostScore
        open %= insertIfNotPresent nb (fromJust newCost `plus` heuristicValue)

analyzeNbs :: ( Ord coord
              , Ord cost
              , Monoid cost
              , Applicative m
              , MonadReader (PathFinderConfig coord cost score) m
              , MonadState (PathFinderState coord cost) m
              ) => coord -> [coord] -> m ()
analyzeNbs predecessor = mapM_ (analyzeNb predecessor)

costFor :: ( Ord coord
           , Functor m
           , MonadState (PathFinderState coord cost) m
           ) => coord -> m (Maybe cost)
costFor c = (fmap . fmap) fst $ Map.lookup c <$> use seen

mayUpdateCost :: ( Ord coord
                 , Ord cost
                 , Functor m
                 , MonadState (PathFinderState coord cost) m
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

visit :: Ord coord => MonadState (PathFinderState coord cost) m => coord -> m ()
visit c = closed %= Set.insert c

alreadyVisited :: ( Ord coord
                  , Functor m
                  , MonadState (PathFinderState coord cost) m) => coord -> m Bool
alreadyVisited c = Set.member c <$> use closed

pathFinderSearch :: forall coord cost score.
                    ( Monoid cost
                    , Monoid score
                    , Ord coord
                    , Ord cost
                    , Ord score
                    ) => PathFinderConfig coord cost score
                      -> coord
                      -> (Maybe (Path cost coord), PathFinderState coord cost)
pathFinderSearch cfg start = runPathFinder cfg initState $ findPath start
  where
    initState :: (PathFinderState coord cost)
    initState = def & seen %~ Map.insert start (mempty,Nothing)
                    & open %~ PSQ.insert start mempty
