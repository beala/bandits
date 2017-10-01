{-# LANGUAGE TemplateHaskell #-}
module Types where

import Lens.Micro.TH
import Lens.Micro
import qualified Statistics.Distribution.Normal as Stat

-- Represents a bandit.
-- `d` is the distribution
data Bandit d = Bandit d

-- Statistics about a given bandit.
data BanditStats = BanditStats { _totalReward :: Double
                               } deriving (Eq, Show)

makeLenses ''BanditStats

instance Monoid BanditStats where
  a `mappend` b = BanditStats (a ^. totalReward + b ^. totalReward)
  mempty = BanditStats 0.0

type BanditDist = Stat.NormalDistribution

data SimState = SimState { _bandits :: [(BanditStats, Bandit BanditDist)] -- The bandits an associated stats
                         , _roundsCount :: Int -- Number of rounds so far
                         }

makeLenses ''SimState

instance Show SimState where
  show simState =
    let stats = simState ^.. (bandits . each . _1) in
      "{ bandits = " ++ show stats ++ ", n = " ++ show (simState ^. roundsCount) ++ "}"

type BanditState = (BanditStats, Bandit BanditDist)

