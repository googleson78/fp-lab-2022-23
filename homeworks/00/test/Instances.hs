{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-dodgy-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Instances () where

import Binary (Binary (..), Bit (..))
import Can (Can (..), LeadingOne (..), canOne, canZero)
import GHC.Generics (Generic)
import Generic.Random (genericArbitraryU)
import Test.QuickCheck (Arbitrary (..), chooseInt, frequency, suchThat, vectorOf)

deriving instance Eq Binary

deriving instance Eq Bit

deriving instance Eq Can

deriving instance Eq LeadingOne

instance Arbitrary Binary where
  arbitrary =
    let bitsToBinary :: [Bit] -> Binary
        bitsToBinary = foldr (flip (:.)) End
     in bitsToBinary <$> do
          n <- chooseInt (0, 16)
          vectorOf n arbitrary

instance Arbitrary Bit where
  arbitrary = do
    bool <- arbitrary
    pure $ if bool then One else Zero

#ifdef CAN
deriving instance Generic LeadingOne
deriving instance Generic Can

instance Arbitrary LeadingOne where
  arbitrary = do
    let nonOne = genericArbitraryU `suchThat` (/= canOne)
    frequency [(1, pure canOne), (4, nonOne)]

instance Arbitrary Can where
  arbitrary =
    frequency [(1, pure canZero), (4, genericArbitraryU `suchThat` (/= canZero))]
#endif
