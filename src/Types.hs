{-# LANGUAGE TemplateHaskell #-}

module Types where

import Lens.Micro.TH
import Lens.Micro
import qualified Statistics.Distribution.Normal as Stat
import qualified Statistics.Distribution as Stat

data Bandit d = Bandit d

instance (Stat.Mean d, Stat.Variance d) => Show (Bandit d) where
  show (Bandit d) = "Bandit {mean = " ++ show (Stat.mean d) ++ ", stdDev = " ++ show (Stat.stdDev d) ++ "}"

distLens :: Lens' (Bandit d) d
distLens = lens (\(Bandit d) -> d) (\_ d -> Bandit d)

meanLens :: Lens' Stat.NormalDistribution Double
meanLens = lens (\d -> Stat.mean d) (\d m -> Stat.normalDistr m (Stat.stdDev d))

-- Statistics about a given bandit.
data BanditStats = BanditStats { _totalReward :: Double
                               , _timesPulled :: Int
                               } deriving (Eq, Show)

makeLenses ''BanditStats

instance Monoid BanditStats where
  a `mappend` b = BanditStats (a ^. totalReward + b ^. totalReward) (a ^. timesPulled + b ^. timesPulled)
  mempty = BanditStats 0.0 0

type BanditDist = Stat.NormalDistribution

data SimState = SimState { _bandits :: [(BanditStats, Bandit BanditDist)] -- ^ The bandits and associated statistics
                         , _roundsCount :: Int -- ^ Number of rounds so far
                         , _regretHist :: [Double]
                         }

makeLenses ''SimState

instance Show SimState where
  show (SimState b count regretHist_) = "SimState { bandits = " ++ show b ++ ", n = " ++ show count ++ ", regret = "++ show regretHist_ ++"}"

type BanditState = (BanditStats, Bandit BanditDist)
