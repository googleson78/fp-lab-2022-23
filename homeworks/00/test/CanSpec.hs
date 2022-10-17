{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}

module CanSpec (canSpec) where

import Test.Hspec
#ifdef CAN
import Can
import Binary
import Test.Hspec.QuickCheck
import Instances ()

canSpec :: Spec
canSpec = describe "Can.hs" do
  prop "forgetLeadingOne always generates a binary with a leading one" do
    (== Just One) . lastBinary . forgetLeadingOne
  prop "only represents canonical binary numbers" do
    not . hasLeadingZero . forget
  prop "forget . Can.canonicalise === Binary.canonicalise" do
    \bin -> forget (Can.canonicalise bin) `shouldBe` Binary.canonicalise bin
  prop "Can.canonicalise . forget === id" do
    \bin -> Can.canonicalise (forget bin) `shouldBe` bin


lastBinary :: Binary -> Maybe Bit
lastBinary End = Nothing
lastBinary (End :. Zero) = Just Zero
lastBinary (End :. One) = Just One
lastBinary (bi :. _) = lastBinary bi
#else
canSpec :: Spec
canSpec = describe "Can.hs" $ it "tests are disabled" $
  pendingWith "To enable them run with \"--flag homework00:can\""
#endif
