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

-- Contraints that allow for random number generation
type BanditM m = (MonadIO m, MonadReader (Rand.Gen (PrimState IO)) m)

-- |Generate a list of random bandits using dist to generate their parameters.
randomBandits ::
  BanditM m
  => Int
  -> BanditDist
  -> m [Bandit BanditDist]
randomBandits count dist =
  replicateM count (randomBandit dist)
  where 
    randomBandit d = do
      gen <- ask
      mean <- liftIO $ Stat.genContVar d gen
      stdDev <- liftIO $ Stat.genContVar d gen
      return $ genNormalBandit mean stdDev
    genNormalBandit avg stdDev = Bandit (Stat.normalDistr avg stdDev)

-- |Pulls the handle and returns the reward
pullArm ::
  BanditM m
  => Bandit BanditDist
  -> m BanditStats
pullArm (Bandit d) = do
  gen <- ask
  reward <- liftIO $ Stat.genContVar d gen
  return $ BanditStats reward 1

simulate ::
  BanditM m
  => SimState -- ^ Starting state
  -> Int      -- ^ Number of rounds
  -> ([BanditState] -> Traversal' [BanditState] (IxValue [BanditState])) -- ^ Choosing function
  -> m SimState
simulate simState 0 _ = return $ simState
simulate simState rounds f = do
  let options = simState ^. bandits
  let Just (chosenStat, chosenBandit) = options ^? f options -- Pick a bandit
  newStat <- pullArm chosenBandit
  let newState = simState
                 & bandits . (f options) . _1 .~ (chosenStat `mappend` newStat) -- Update stats
                 & roundsCount %~ (+1) -- Update rounds count
  simulate newState (rounds - 1) f
  
initState ::
  BanditM m
  => Int -- Number of bandits
  -> m SimState
initState count = do
  initBandits <- randomBandits count (Stat.normalDistr 100 1)
  let initStats = replicate count mempty
  return $ SimState (zip initStats initBandits) 0

regret :: SimState -> Double
regret simState =
  let allTimesPulled = fromIntegral <$> (simState ^.. bandits . each . _1 . timesPulled)
      allMeans = simState ^.. bandits . each . _2 . distLens . meanLens
      expectedReturn = sum (zipWith (*) allTimesPulled allMeans)
      maxMean = maximum (simState ^.. bandits . each . _2 . distLens . meanLens)
      rounds = fromIntegral $ simState ^. roundsCount
      maxExpectedReturn = maxMean * rounds
  in
    maxExpectedReturn - expectedReturn
    
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
  
