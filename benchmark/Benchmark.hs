module Main where

import Criterion.Main

import Math.NumberTheory.Powers.IntegerBench as Integer
import Math.NumberTheory.Powers.NaturalBench as Natural

main :: IO ()
main = defaultMain
  [ Integer.benchSuite
  , Natural.benchSuite
  ]
