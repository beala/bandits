{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

-- import Lib
import qualified Statistics.Distribution as Stat
import qualified Statistics.Distribution.Normal as Stat
import Control.Monad.Primitive
import qualified System.Random.MWC as Rand
import Control.Monad.State.Strict
import Control.Monad.Reader
import Lens.Micro
import Lens.Micro.Internal
import Types

type BanditM m = (MonadIO m, MonadReader (Rand.Gen (PrimState IO)) m)

genNormalBandit :: Double -> Double -> Bandit Stat.NormalDistribution
genNormalBandit avg stdDev = Bandit (Stat.normalDistr avg stdDev)

-- |Generate a list of random bandits using dist to generate their parameters.
randomBandits ::
  BanditM m
  => Int
  -> BanditDist
  -> m [Bandit BanditDist]
randomBandits count dist =
  replicateM count (randomBandit dist)

randomBandit ::
  BanditM m
  => Stat.NormalDistribution
  -> m (Bandit BanditDist)
randomBandit d = do
  gen <- ask
  mean <- liftIO $ Stat.genContVar d gen
  stdDev <- liftIO $ Stat.genContVar d gen
  return $ genNormalBandit mean stdDev

-- |Pulls the handle and returns the reward
pullHandle ::
  ( Stat.ContGen d
  , MonadIO m
  , MonadReader (Rand.Gen (PrimState IO)) m)
  => Bandit d
  -> m Double
pullHandle (Bandit d) = do
  gen <- ask
  liftIO $ Stat.genContVar d gen

pullHandleStats ::
  ( MonadIO m
  , MonadReader (Rand.Gen (PrimState IO)) m)
  => Bandit BanditDist
  -> m BanditStats
pullHandleStats bandit = do
  reward <- pullHandle bandit
  return $ BanditStats reward

simulate ::
  ( MonadIO m
  , MonadReader (Rand.Gen (PrimState IO)) m)
  => SimState
  -> Int -- Number of rounds
  -> ([BanditState] -> Traversal' [BanditState] (IxValue [BanditState]))
  -> m SimState
simulate simState 0 _ = return $ simState
simulate simState rounds f = do
  let options = simState ^. bandits
  let Just (chosenStat, chosenBandit) = options ^? f options
  reward <- pullHandleStats chosenBandit
  let newState = simState & bandits . (f options) .~ (chosenStat `mappend` reward, chosenBandit)
  let newState' = newState & roundsCount %~ (+ 1)
  simulate newState' (rounds - 1) f
  
initState ::
  BanditM m
  => Int -- Number of bandits
  -> m SimState
initState count = do
  initBandits <- randomBandits count (Stat.normalDistr 5 1)
  let initStats = replicate count mempty
  return $ SimState (zip initStats initBandits) 0

regret :: SimState -> Double
regret _ = 0.0

run ::
  Int
  -> Int
  -> ([BanditState] -> Traversal' [BanditState] (IxValue [BanditState]))
  -> ReaderT (Rand.Gen (PrimState IO)) IO SimState
run rounds count f = do
  simState <- initState count
  simulate simState rounds f

choose :: [BanditState] -> Traversal' [BanditState] (IxValue [BanditState])
choose _ = ix 1

main :: IO ()
main = do
  rand <- Rand.createSystemRandom
  finalState <- runReaderT (run 1000 10 choose) rand
  let totalRegret = regret finalState
  putStrLn $ show finalState
  putStrLn $ show totalRegret
  
