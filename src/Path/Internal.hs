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

type HeuristicScore = Double
type WayCost = Double

data Path l c = Path { _pathLength :: !l
                     , _pathCoords :: [c]
                     } deriving (Show,Eq)
makeLenses ''Path

type PredecessorMap c = Map.Map c (WayCost,Maybe c)

data PathFinderState c = PathFinderState { _closed :: Set.Set c
                                         , _open :: PSQ.PSQ c HeuristicScore
                                         , _seen :: PredecessorMap c
                                         }
makeLenses ''PathFinderState


instance Ord c => Default (PathFinderState c) where
  def = PathFinderState def PSQ.empty def

data PathFinderConfig c n = PathFinderConfig { _canBeWalked :: c -> Bool
                                             , _heuristicCost :: c -> n
                                             , _stepCost :: c -> c -> n
                                             , _neighbors :: c -> [c]
                                             , _isGoal :: c -> Bool
                                             }
makeLenses ''PathFinderConfig

newtype PathFinder c a = PathFinder (ReaderT (PathFinderConfig c Double) (
                                      StateT (PathFinderState c) Identity) a)
    deriving ( Functor, Applicative, Monad
             , MonadState (PathFinderState c), MonadReader (PathFinderConfig c Double))

runPathFinder :: PathFinderConfig c Double ->
                 PathFinderState c ->
                 PathFinder c a ->
                 (a,PathFinderState c)
runPathFinder c st (PathFinder a) = runIdentity $ runStateT (runReaderT a c) st

execPathFinder :: PathFinderConfig c Double ->
                  PathFinderState c ->
                  PathFinder c a ->
                  PathFinderState c
execPathFinder c st a = snd $ runPathFinder c st a

evalPathFinder :: PathFinderConfig c Double ->
                  PathFinderState c ->
                  PathFinder c a ->
                  a
evalPathFinder c st a = fst $ runPathFinder c st a

findPath :: Ord c => c -> PathFinder c (Maybe (Path Double c))
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

visitAndExpand :: ( Ord c
                  , Applicative m
                  , MonadState (PathFinderState c) m
                  , MonadReader (PathFinderConfig c Double) m
                  ) => c -> m ()
visitAndExpand c = visit c >> expand c >>= analyzeNbs c

reconstructPath :: Ord c => c -> PredecessorMap c -> Maybe (Path Double c)
reconstructPath finish pmap = do
  (totalCost,predec) <- Map.lookup finish pmap
  case predec of
    Nothing -> return $ Path 0 []
    Just _ -> return $ Path totalCost (reverse $ go finish)
  where
    -- go :: Ord a => a -> [a]
    go current = case Map.lookup current pmap of
      Nothing -> []
      Just (_, Just predec) -> current : go predec
      Just (_, Nothing) -> [current]

nodesLeftToExpand :: (Functor m, MonadState (PathFinderState c) m) => m Int
nodesLeftToExpand = PSQ.size <$> use open

expand :: ( Applicative m
          , MonadReader (PathFinderConfig c Double) m
          ) => c -> m [c]
expand coord = filter <$> view canBeWalked <*> (view neighbors <*> pure coord)

analyzeNb :: ( Ord c
             , Applicative m
             , MonadReader (PathFinderConfig c Double) m
             , MonadState (PathFinderState c) m
             ) => c -> c -> m ()
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

analyzeNbs :: ( Ord c
              , Applicative m
              , MonadReader (PathFinderConfig c Double) m
              , MonadState (PathFinderState c) m
              ) => c -> [c] -> m ()
analyzeNbs predecessor = mapM_ (analyzeNb predecessor)

costFor :: ( Ord c
           ,Functor m
           , MonadState (PathFinderState c) m
           ) => c -> m (Maybe WayCost)
costFor c = (fmap . fmap) fst $ Map.lookup c <$> use seen

mayUpdateCost :: (Ord c, Functor m, MonadState (PathFinderState c) m) =>
              Maybe WayCost -> c -> c -> m ()
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

visit :: Ord c => MonadState (PathFinderState c) m => c -> m ()
visit c = closed %= Set.insert c

alreadyVisited :: (Ord c, Functor m, MonadState (PathFinderState c) m) => c -> m Bool
alreadyVisited c = Set.member c <$> use closed
