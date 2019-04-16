-- |
-- Module:      Math.NumberTheory.TestUtils
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Math.NumberTheory.TestUtils
  ( module Test.SmallCheck.Series
  , Power (..)
  , Huge (..)
  , PNO (..)
  , testSmallAndQuick
  ) where

import Control.Monad (guard)
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC hiding (Positive, NonNegative, generate, getNonNegative)
import Test.SmallCheck.Series (Positive(..), NonNegative(..), Serial(..), Series, generate)

import Control.Applicative

testSmallAndQuick
  :: SC.Testable IO a
  => QC.Testable a
  => String -> a -> TestTree
testSmallAndQuick name f = testGroup name
  [ SC.testProperty "smallcheck" f
  , QC.testProperty "quickcheck" f
  ]

-------------------------------------------------------------------------------
-- Power

newtype Power a = Power { getPower :: a }
  deriving (Eq, Ord, Read, Show, Num, Enum, Bounded, Integral, Real)

instance (Monad m, Num a, Ord a, Serial m a) => Serial m (Power a) where
  series = Power <$> series `suchThatSerial` (> 0)

instance (Num a, Ord a, Integral a, Arbitrary a) => Arbitrary (Power a) where
  arbitrary = Power <$> (getSmall <$> arbitrary) `suchThat` (> 0)
  shrink (Power x) = Power <$> filter (> 0) (shrink x)

suchThatSerial :: Series m a -> (a -> Bool) -> Series m a
suchThatSerial s p = s >>= \x -> if p x then pure x else empty

-------------------------------------------------------------------------------
-- Huge

newtype Huge a = Huge { getHuge :: a }
  deriving (Eq, Ord, Read, Show, Num, Enum, Bounded, Integral, Real)

instance (Num a, Arbitrary a) => Arbitrary (Huge a) where
  arbitrary = do
    Positive l <- arbitrary
    ds <- vector (l :: Int)
    return $ Huge $ foldl1 (\acc n -> acc * 2^(63 :: Int) + n) ds

-- | maps 'Huge' constructor over series
instance Serial m a => Serial m (Huge a) where
  series = fmap Huge series

-------------------------------------------------------------------------------
-- PositiveNotOne

newtype PNO = PNO { getPno :: Rational }
  deriving (Eq, Ord, Read, Show, Num)

instance Arbitrary PNO where
  arbitrary = fmap PNO $ arbitrary `suchThat` \r -> r > 1

instance Monad m => Serial m PNO where
  series = do
    r <- series
    guard (r > 1)
    return (PNO r)

-------------------------------------------------------------------------------
-- Positive from smallcheck

instance (Num a, Ord a, Arbitrary a) => Arbitrary (Positive a) where
  arbitrary = Positive <$> (arbitrary `suchThat` (> 0))
  shrink (Positive x) = Positive <$> filter (> 0) (shrink x)
