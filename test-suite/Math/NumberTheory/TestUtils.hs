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
{-# LANGUAGE StandaloneDeriving         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Math.NumberTheory.TestUtils
  ( module Test.SmallCheck.Series
  , Power (..)
  , testSmallAndQuick
  ) where

import Test.SmallCheck.Series (cons2)
import Test.Tasty
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
-- Serial monadic actions

instance Monad m => Serial m Word where
  series =
    generate (\d -> if d >= 0 then pure 0 else empty) <|> nats
    where
      nats = generate $ \d -> if d > 0 then [1 .. fromInteger (toInteger d)] else empty

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
-- Positive from smallcheck

instance (Num a, Ord a, Arbitrary a) => Arbitrary (Positive a) where
  arbitrary = Positive <$> (arbitrary `suchThat` (> 0))
  shrink (Positive x) = Positive <$> filter (> 0) (shrink x)
