-- |
-- Module:      Math.NumberTheory.Powers.NaturalTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
--
-- Tests for Math.NumberTheory.Powers.Natural
--

{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.Powers.NaturalTests
  ( testSuite
  ) where

import Test.Tasty

#if MIN_VERSION_base(4,8,0)
#else
import Data.Word
#endif

import Math.NumberTheory.Powers.Natural
import Numeric.Natural
import Math.NumberTheory.TestUtils

-- Arbitrary Natural
import Orphans ()

-- | Check that 'naturalPower' == '^'.
naturalPowerProperty :: Natural -> Power Int -> Bool
naturalPowerProperty a (Power b) = naturalPower a b == a ^ b

-- | Check that 'naturalWordPower' == '^'.
naturalWordPowerProperty :: Natural -> Power Word -> Bool
naturalWordPowerProperty a (Power b) = naturalWordPower a b == a ^ b

testSuite :: TestTree
testSuite = testGroup "Natural"
  [ testSmallAndQuick "naturalPower"     naturalPowerProperty
  , testSmallAndQuick "naturalWordPower" naturalWordPowerProperty
  ]
