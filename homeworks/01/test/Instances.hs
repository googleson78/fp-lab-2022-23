{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-dodgy-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Instances (AThrice (..)) where

import Data.Monoid (All (All, getAll))
import GHC.Generics (Generic)
import Generic.Random (genericArbitraryU)
import Matrix
import Test.QuickCheck (Arbitrary (..), CoArbitrary)
import Test.QuickCheck.Arbitrary (vector)
import Test.QuickCheck.Function
import Text.Printf (printf)
import TicTacToe
import Utils

newtype AThrice a = AThrice (Thrice a)

instance Show a => Show (AThrice a) where
  show (AThrice t) = printf "%s %s %s" (show $ t Zero) (show $ t One) (show $ t Two)

instance Arbitrary a => Arbitrary (AThrice a) where
  arbitrary = AThrice . tupToThrice <$> arbitrary

deriving instance Generic Three

instance Arbitrary Three where
  arbitrary = genericArbitraryU

instance Function Three

instance CoArbitrary Three

deriving instance Ord Three

instance Eq a => Eq (Matrix a) where
  (==) (MkMatrix m1) (MkMatrix m2) =
    getAll $ foldThrice \i -> foldThrice \j -> All (m1 i j == m2 i j)

instance Arbitrary a => Arbitrary (Matrix a) where
  arbitrary = listToMatrix <$> vector 9

deriving instance Generic Marker

instance Arbitrary Marker where
  arbitrary = genericArbitraryU

deriving instance Generic Result

instance Arbitrary Result where
  arbitrary = genericArbitraryU

deriving instance Eq Result
