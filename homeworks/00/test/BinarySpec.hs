{-# LANGUAGE BlockArguments #-}

module BinarySpec (binarySpec) where

import Binary
import Instances ()
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (NonNegative (..))

binarySpec :: Spec
binarySpec = describe "Binary.hs" do
  succBinarySpec
  hasLeadingZeroSpec
  canonicaliseSpec
  integerToBinarySpec
  binaryToIntegerSpec
  addBinarySpec
  properties

succBinarySpec :: Spec
succBinarySpec = describe "succBinary" do
  it "works on End" do
    succBinary End `shouldBe` End :. One
  it "works on Zero" do
    succBinary (End :. Zero) `shouldBe` End :. One
  it "works on One" do
    succBinary (End :. One) `shouldBe` End :. One :. Zero
  it "works on 8" do
    succBinary (End :. One :. One :. One) `shouldBe` End :. One :. Zero :. Zero :. Zero

hasLeadingZeroSpec :: Spec
hasLeadingZeroSpec = describe "hasLeadingZero" do
  it "works on End" do
    not $ hasLeadingZero End
  it "works on End :. One" do
    not $ hasLeadingZero $ End :. One
  it "works on End :. Zero" do
    hasLeadingZero $ End :. Zero
  it "works on End :. Zero :. One :. Zero" do
    hasLeadingZero $ End :. Zero :. One :. Zero

canonicaliseSpec :: Spec
canonicaliseSpec = describe "canonicalise" do
  it "works on End" do
    canonicalise End `shouldBe` End
  it "works on End :. One" do
    canonicalise (End :. One) `shouldBe` End :. One
  it "works on End :. Zero" do
    canonicalise (End :. Zero) `shouldBe` End
  it "works on End :. Zero :. One :. Zero" do
    canonicalise (End :. Zero :. One :. Zero) `shouldBe` End :. One :. Zero

integerToBinarySpec :: Spec
integerToBinarySpec = describe "integerToBinarySpec" do
  it "works on 0" do
    integerToBinary 0 `shouldBe` End
  it "works on 69" do
    integerToBinary 69 `shouldBe` End :. One :. Zero :. Zero :. Zero :. One :. Zero :. One
  it "works on 5" do
    integerToBinary 5 `shouldBe` End :. One :. Zero :. One
  it "works on 7" do
    integerToBinary 7 `shouldBe` End :. One :. One :. One

binaryToIntegerSpec :: Spec
binaryToIntegerSpec = describe "binaryToIntegerSpec" do
  it "works on 0" do
    binaryToInteger End `shouldBe` 0
  it "works on 69" do
    binaryToInteger (End :. One :. Zero :. Zero :. Zero :. One :. Zero :. One) `shouldBe` 69
  it "works on 16" do
    binaryToInteger (End :. One :. Zero :. Zero :. Zero :. Zero) `shouldBe` 16
  it "works with trailing zeroes" do
    binaryToInteger (End :. Zero :. One) `shouldBe` 1

addBinarySpec :: Spec
addBinarySpec = describe "addBinary" do
  it "works on End (End :. One)" do
    addBinary End (End :. One) `shouldBe` End :. One
  it "works on (End :. Zero) (End :. One)" do
    addBinary (End :. Zero) (End :. One) `shouldBe` End :. One
  it "works on (End :. Zero) (End :. One)" do
    addBinary (End :. One) (End :. Zero) `shouldBe` End :. One
  it "works on (End :. One :. Zero) (End :. One :. Zero :. Zero)" do
    addBinary (End :. One :. Zero) (End :. One :. Zero :. Zero) `shouldBe` End :. One :. One :. Zero

properties :: Spec
properties = describe "properties" do
  describe "succBinary" do
    prop "increases the number by one according to binaryToInteger" \bin ->
      binaryToInteger (succBinary bin) `shouldBe` (1 + binaryToInteger bin)
  describe "canonicalise" do
    prop "drops trailing zeroes" $ not . hasLeadingZero . canonicalise
    prop "doesn't change the number according to binaryToInteger" \bin ->
      binaryToInteger (canonicalise bin) `shouldBe` binaryToInteger bin
  describe "integerToBinary" do
    prop "generates canonical vectors" \(NonNegative n) ->
      not $ hasLeadingZero $ integerToBinary n
  describe "addBinary" do
    prop "behaves like + on integers numbers" \bin1 bin2 ->
      binaryToInteger (addBinary bin1 bin2) `shouldBe` binaryToInteger bin1 + binaryToInteger bin2
  describe "conversions" do
    prop "integerToBinary . binaryToInteger === canonicalise" \bin ->
      integerToBinary (binaryToInteger bin) `shouldBe` canonicalise bin
    prop "binaryToInteger . integerToBinary === id" \(NonNegative n) ->
      binaryToInteger (integerToBinary n) `shouldBe` n
