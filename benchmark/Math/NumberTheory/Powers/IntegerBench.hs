{-# LANGUAGE RankNTypes #-}

module Math.NumberTheory.Powers.IntegerBench
  ( benchSuite
  ) where

import Criterion.Main

import Math.NumberTheory.Powers.Integer

bases :: [Integer]
bases = [2, 10]

powers :: [Int]
powers = concatMap (\x -> [x - 1, x, x + 1]) (take 8 (iterate (* 2) 8))

benchBaseAndPower :: Integer -> Int -> Benchmark
benchBaseAndPower base power = bgroup (show base ++ "^" ++ show power)
  [ bench "integerPower"     $ nf (integerPower base) power
  , bench "Integer ^ Int"    $ nf (base ^) power
  -- , bench "integerWordPower" $ nf (integerWordPower base) (fromIntegral power)
  -- , bench "Integer ^ Word"   $ nf (base ^) (fromIntegral power :: Word)
  ]

benchSuite :: Benchmark
benchSuite = bgroup "Integer" $ [ benchBaseAndPower base power | base <- bases, power <- powers ]
