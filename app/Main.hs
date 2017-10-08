{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Statistics.Distribution as Stat
import qualified Statistics.Distribution.Normal as Stat
import qualified Statistics.Distribution.Uniform as Stat
import Control.Monad.Primitive
import qualified System.Random.MWC as Rand
import Control.Monad.State.Strict
import Control.Monad.Reader
import Lens.Micro
import Types
import qualified Data.List as List
import Options.Generic

-- Contraints that allow for random number generation
type BanditM m = (MonadIO m, MonadReader (Rand.Gen (PrimState IO)) m)

-- |Generate a list of random bandits using dist to generate their parameters.
randomBandits ::
  (BanditM m, Stat.ContGen d1, Stat.ContGen d2)
  => Int
  -> d1
  -> d2
  -> m [Bandit BanditDist]
randomBandits count avgDist stdDevDist =
  replicateM count randomBandit
  where 
    randomBandit = do
      gen <- ask
      mean <- liftIO $ Stat.genContVar avgDist gen
      stdDev <- liftIO $ Stat.genContVar stdDevDist gen
      return $ Bandit (Stat.normalDistr mean (abs stdDev))

-- |Pulls the handle and returns the reward
pullArm ::
  (BanditM m, Stat.ContGen d)
  => Bandit d
  -> m BanditStats
pullArm (Bandit d) = do
  gen <- ask
  reward <- liftIO $ Stat.genContVar d gen
  return $ BanditStats reward 1

simulate ::
  BanditM m
  => SimState -- ^ Starting state
  -> Int      -- ^ Number of rounds
  -> (SimState -> m Int) -- ^ Choosing function
  -> m SimState
simulate simState 0 _ = return simState
simulate simState rounds f = do
  chosenIndex <- f simState
  let Just (chosenStat, chosenBandit) = simState ^? bandits. ix chosenIndex -- Pick a bandit
  newStat <- pullArm chosenBandit
  let newState = simState
                 & (bandits . ix chosenIndex) . _1 .~ (chosenStat `mappend` newStat) -- Update stats
                 & roundsCount %~ (+1) -- Update rounds count
                 & regretHist %~ (\t -> (regret simState) : t)
  simulate newState (rounds - 1) f

-- | Initialize the state but creating the specified number of bandits and pulling each
-- | one once.
initState ::
  BanditM m
  => Int -- Number of bandits
  -> m SimState
initState count = do
  initBandits <- randomBandits count (Stat.uniformDistr 0 50) (Stat.normalDistr 0 5)
  let initStats = replicate count mempty
  return $ SimState (zip initStats initBandits) 0 []

regret :: SimState -> Double
regret simState =
  let allTimesPulled = fromIntegral <$> (simState ^.. bandits . each . _1 . timesPulled)
      allMeans = simState ^.. bandits . each . _2 . distLens . meanLens
      actualExpectedReturn = sum (zipWith (*) allTimesPulled allMeans)
      maxMean = maximum (simState ^.. bandits . each . _2 . distLens . meanLens)
      rounds = fromIntegral $ simState ^. roundsCount
      maxExpectedReturn = maxMean * rounds
  in
    maxExpectedReturn - actualExpectedReturn

type Concrete a = (ReaderT (Rand.Gen (PrimState IO)) IO) a

run ::
  Int
  -> Int
  -> (SimState -> Concrete Int)
  -> Concrete SimState
run rounds count f = do
  startingState <- initState count
  simulate startingState rounds f

-- | Return the index of the largest item as determined by the function
argMaxIndex :: (Ord b, Num b) => [a] -> (a -> b) -> Maybe Int
argMaxIndex as f =
  let magnitudes = f <$> as
      indexAndMagnitude = zip [0..] magnitudes
      biggestToSmallest = (List.sortOn (negative . snd) indexAndMagnitude)
  in biggestToSmallest ^? _head . _1
  where 
    negative x = -x -- Swap the sign, so it's sorted biggest to smallest.

-- | Choose the bandit with the highest Upper Confidence Bound
choose :: SimState -> Int
choose simState =
  let banditStats = simState ^.. bandits . each . _1
      curRound = simState ^. roundsCount
      Just maxIndex = argMaxIndex banditStats (ucb curRound)
  in maxIndex

ucb :: Int -> BanditStats -> Double
ucb curRound banditStats =
  let totalTimesPulled = fromIntegral (banditStats ^. timesPulled)
      empiricalMean = (banditStats ^. totalReward) / totalTimesPulled
      t = fromIntegral curRound
      confidence = sqrt ((2.0 * log (1 + (t * ((log t) ^ 2)))) / totalTimesPulled)
      in
    if totalTimesPulled /= 0 then empiricalMean + confidence else maxDouble
  where
    maxDouble = 100000000 -- TODO: What's the actual max double?

-- | Choose a random bandit.
chooseRandom :: BanditM m => SimState -> m Int
chooseRandom simState = do
    let count = fromIntegral $ length $ simState ^. bandits
    let d = Stat.uniformDistr 0 (count - 1)
    gen <- ask
    rand <- liftIO $ Stat.genContVar d gen
    return $ truncate rand

-- | Pick the bandit with the best empirical mean.
chooseBestMean :: SimState -> Int
chooseBestMean simState =
  let banditStats = simState ^.. bandits . each . _1
      Just maxIndex = argMaxIndex banditStats avgReturn
  in
    maxIndex

-- | Pick the bandit with the highest mean reward.
godMode :: SimState -> Int
godMode simState =
  let bandits_ = simState ^.. bandits . each . _2
      Just maxIndex = argMaxIndex bandits_ (\(Bandit d) -> Stat.mean d)
  in
    maxIndex

avgReturn :: BanditStats -> Double
avgReturn (BanditStats reward pulled) = reward / (fromIntegral pulled)

main :: IO ()
main = do
  (opts :: BanditOpts Unwrapped) <-unwrapRecord "Multi-armed bandit simulator."
  rand <- Rand.createSystemRandom
  finalState <- runReaderT (run (optRounds opts) (optBandits opts) (return . godMode)) rand
  _ <- traverse (putStrLn . show) (reverse (finalState ^. regretHist))
  return ()
  

data BanditOpts w =
  BanditOpts { optRounds :: w ::: Int <?> "Number of rounds to simulate."
             , optBandits :: w ::: Int <?> "Number of bandits."
             -- , optMeanMean :: w ::: Double <?> "Mean of the mean."
             -- , optStdDevMean :: w ::: Double <?> "StdDev of the mean."
             -- , optMeanStdDev :: w ::: Double <?> "Mean of the StdDev."
             -- , optStdDevStdDev :: w ::: Double <?> "StdDev of the StdDev."
             } deriving (Generic)

instance ParseRecord (BanditOpts Wrapped)
