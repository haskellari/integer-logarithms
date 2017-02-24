{-# LANGUAGE RankNTypes #-}

module Math.NumberTheory.Powers.NaturalBench
  ( benchSuite
  ) where

import Criterion.Main
import Numeric.Natural
import Math.NumberTheory.Powers.Natural

bases :: [Natural]
bases = [2, 10]

powers :: [Int]
powers = concatMap (\x -> [x - 1, x, x + 1]) (take 8 (iterate (* 2) 8))

benchBaseAndPower :: Natural -> Int -> Benchmark
benchBaseAndPower base power = bgroup (show base ++ "^" ++ show power)
  [ bench "naturalPower"     $ nf (naturalPower base) power
  , bench "Natural ^ Int"    $ nf (base ^) power
  -- , bench "naturalWordPower" $ nf (naturalWordPower base) (fromIntegral power)
  -- , bench "Natural ^ Word"   $ nf (base ^) (fromIntegral power :: Word)
  ]

benchSuite :: Benchmark
benchSuite = bgroup "Natural" $ [ benchBaseAndPower base power | base <- bases, power <- powers ]
