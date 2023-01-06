-- because of matrix test formatting..
{- ORMOLU_DISABLE -}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module MatrixSpec (matrixSpec) where

import Data.Monoid (All (..), Any (..))
import Instances (AThrice (..))
import Matrix
import TicTacToe
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop, modifyMaxSuccess)
import Utils
import Data.Maybe (mapMaybe)
import Data.List (intercalate)
import Control.Applicative (ZipList(..))

matrixSpec :: Spec
matrixSpec = describe "Matrix" do
  constantMatrixSpec
  addMatrixSpec
  ixSpec
  getRowSpec
  getColSpec
  getDiagSpec
  getOtherDiagSpec
  transposeSpec

  foldThriceWithSpec
  foldMatrixWithSpec
  eqMatrixSpec
  imapThriceSpec
  imapMatrixSpec
  placeSpec
  concatThriceWithSpec
  concatMatrixSpec

constantMatrixSpec :: Spec
constantMatrixSpec = describe "constantMatrix" do
  prop "correctness via lists" \(n :: Integer) ->
    constantMatrix n
      `shouldBe` listToMatrix (replicate 9 n)

addMatrixSpec :: Spec
addMatrixSpec = describe "addMatrix" do
  it "works on an example" do
    let i1 =
          matrix
            0 1 2
            3 4 5
            6 7 8
    let i2 =
          matrix
            (-0) (-1) (-2)
            (-3) (-4) (-5)
            (-6) (-7) (-8)
    let res = constantMatrix 0
    addMatrix i1 i2 `shouldBe` res
  prop "correctness via lists" \m1 m2 ->
    (m1 `addMatrix` m2)
      `shouldBe` listToMatrix (zipWith (+) (matrixToList m1) (matrixToList m2))

ixSpec :: Spec
ixSpec = describe "ixSpec" do
  it "works on an example" do
    let m =
          matrix
            0 1 2
            3 4 5
            6 7 8
    ix One One m `shouldBe` (4 :: Integer)
  prop "correctness via lists" \(m :: Matrix Integer) i j ->
    ix i j m
      `shouldBe` ixListAsIfMatrix i j (matrixToList m)

getRowSpec :: Spec
getRowSpec = describe "getRowSpec" do
  it "works on an example" do
    let m =
          matrix
            0 1 2
            3 4 5
            6 7 8
    thriceToList (getRow One m) `shouldBe` [3, 4, 5 :: Integer]
  prop "correctness via lists" \(m :: Matrix Integer) i ->
    foldThrice (\j -> [getRow i m j])
      `shouldBe` take 3 (drop (threeToInt i * 3) $ matrixToList m)

getColSpec :: Spec
getColSpec = describe "getColSpec" do
  it "works on an example" do
    let m =
          matrix
            0 1 2
            3 4 5
            6 7 8
    thriceToList (getCol One m) `shouldBe` [1, 4, 7 :: Integer]
  prop "correctness via lists" \(m :: Matrix Integer) i ->
    foldThrice (\j -> [getCol i m j])
      `shouldBe` let matrixList = matrixToList m
                     iint = threeToInt i
                  in map (\j -> matrixList !! (3 * j + iint)) [0, 1, 2]

getDiagSpec :: Spec
getDiagSpec = describe "getDiagSpec" do
  it "works on an example" do
    let m =
          matrix
            0 1 2
            3 4 5
            6 7 8
    thriceToList (getDiag m) `shouldBe` [0, 4, 8 :: Integer]
  prop "correctness via lists" \(m :: Matrix Integer) ->
    foldThrice (\j -> [getDiag m j])
      `shouldBe` let matrixList = matrixToList m
                  in map (matrixList !!) [0, 4, 8]

getOtherDiagSpec :: Spec
getOtherDiagSpec = describe "getOtherDiagSpec" do
  it "works on an example" do
    let m =
          matrix
            0 1 2
            3 4 5
            6 7 8
    thriceToList (getOtherDiag m) `shouldBe` [2, 4, 6 :: Integer]
  prop "correctness via lists" \(m :: Matrix Integer) ->
    foldThrice (\j -> [getOtherDiag m j])
      `shouldBe` let matrixList = matrixToList m
                  in map (matrixList !!) [2, 4, 6]

transposeSpec :: Spec
transposeSpec = describe "transposeSpec" do
  prop "transpose is idempotent (transpose . transpose == id)" \(m :: Matrix Int) ->
    transpose (transpose m) `shouldBe` m
  prop "correctness via lists" \(MkMatrix @Int m) ->
    matrixToList (transpose (MkMatrix m))
    `shouldBe`
      concat (getZipList $ traverse (ZipList . thriceToList) $ thriceToList m)

foldThriceWithSpec :: Spec
foldThriceWithSpec = describe "foldThriceWithSpec" do
  prop "correctness via All(&&)" \(AThrice t) ->
    foldThriceWith
      (&&)
      t
      `shouldBe` getAll (foldMapThrice All t)
  prop "correctness via Any(||)" \(AThrice t) ->
    foldThriceWith
      (||)
      t
      `shouldBe` getAny (foldMapThrice Any t)
  prop "correctness via lists(++)" \(AThrice @[Int] t) ->
    foldThriceWith
      (++)
      t
      `shouldBe` foldThrice t

foldMatrixWithSpec :: Spec
foldMatrixWithSpec = describe "foldMatrixWithSpec" do
  prop "correctness via All(&&)" \m ->
    foldMatrixWith
      (&&)
      m
      `shouldBe` getAll (foldMapMatrix All m)
  prop "correctness via Any(||)" \m ->
    foldMatrixWith
      (||)
      m
      `shouldBe` getAny (foldMapMatrix Any m)
  prop "correctness via lists(++)" \(m :: Matrix [Int]) ->
    foldMatrixWith
      (++)
      m
      `shouldBe` foldMapMatrix id m

eqMatrixSpec :: Spec
eqMatrixSpec = describe "eqMatrixSpec" do
  modifyMaxSuccess (const 200000) $ prop "correctness via lists" \(m1 :: Matrix Marker) m2 ->
    eqMatrix (==) m1 m2
    `shouldBe`
      matrixToList m1 == matrixToList m2

imapThriceSpec :: Spec
imapThriceSpec = describe "imapThriceSpec" do
  prop "correctness via lists" \(AThrice t) ->
    let f i x = threeToInt i * x
     in
    thriceToList (imapThrice f t)
    `shouldBe` map (uncurry f) (thriceToiList t)

imapMatrixSpec :: Spec
imapMatrixSpec = describe "imapMatrixSpec" do
  prop "correctness via lists" \m ->
    let f i j x = threeToInt i * x * threeToInt j
     in
    matrixToList (imapMatrix f m)
    `shouldBe` map (\(i, j, x) -> f i j x) (matrixToiList m)

placeSpec :: Spec
placeSpec = describe "placeSpec" do
  prop "ix after place returns the set item" \m i j (x :: Integer) ->
    ix i j (place i j x m) `shouldBe` x

  prop "place doesn't modify any other items" \m i j (x :: Integer) ->
    let unchangedToList :: Matrix a -> [(Three, Three, a)]
        unchangedToList = mapMaybe (\(i1, j1, y) -> if i == i1 && j == j1 then Nothing else Just (i1, j1, y)) . matrixToiList
     in unchangedToList (place i j x m) `shouldBe` unchangedToList m

concatThriceWithSpec :: Spec
concatThriceWithSpec = describe "concatThriceWithSpec" do
  prop "correctness via lists" \(AThrice t) str ->
    concatThriceWith str t `shouldBe` intercalate str (thriceToList t)

concatMatrixSpec :: Spec
concatMatrixSpec = describe "concatMatrixSpec" do
  prop "correctness via lists" \(MkMatrix m) str1 str2 ->
    concatMatrixWith str1 str2 (MkMatrix m) `shouldBe`
      intercalate str1 (map (intercalate str2 . thriceToList) (thriceToList m))
