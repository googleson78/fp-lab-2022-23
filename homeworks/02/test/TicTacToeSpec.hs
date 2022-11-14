-- because of matrix test formatting..
{- ORMOLU_DISABLE -}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TicTacToeSpec (ticTacToeSpec) where

import Data.Foldable (for_)
import Instances ()
import Matrix
import TicTacToe
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Utils
import Data.List (sort)
import Test.QuickCheck.Property ((==>))
import Text.Printf (printf)
import Data.Maybe (mapMaybe, isNothing)

ticTacToeSpec :: Spec
ticTacToeSpec = describe "TicTacToe" do
  joinSpec
  checkThreeSpotsSpec
  winnerSpec
  emptySpotsSpec

joinSpec :: Spec
joinSpec = describe "joinSpec" do
  prop "should be associative" \br1 br2 br3 ->
    join (join br1 br2) br3 `shouldBe` join br1 (join br2 br3)

  prop ("should have " ++ show Full ++ " as a bottom value") \br -> do
    join Full br `shouldBe` br
    join br Full `shouldBe` br

  prop "should always choose Wins over non-Wins" \m br -> not (isWin br) ==> do
    let brWins = Wins m
    join brWins br `shouldBe` brWins
    join br brWins `shouldBe` brWins

checkThreeSpotsSpec :: Spec
checkThreeSpotsSpec = describe "checkThreeSpotsSpec" do
  for_ [X, O] \marker -> it (printf "works on a winning example for %s" (show marker)) do
    let row = let x = Just marker in thrice x x x
    checkThreeSpots row `shouldBe` Wins marker
  it "works on an example with an empty spot" do
    let row = thrice Nothing (Just X) (Just O)
    checkThreeSpots row `shouldBe` HasEmpty
  it "works on an example with a full board" do
    let row = thrice (Just O) (Just X) (Just O)
    checkThreeSpots row `shouldBe` Full

winnerSpec :: Spec
winnerSpec = describe "winnerSpec" do
  it (printf "returns %s for an empty board" (show HasEmpty)) $
   winner (constantMatrix Nothing) `shouldBe` HasEmpty
  for_ [X, O] \marker -> for_ [Zero, One, Two] \winIndex -> do
    it (printf "detects wins on row %s for %s" (show winIndex) (show marker)) do
      let board =
            MkMatrix \i _ ->
              if i == winIndex then Just marker else Nothing
      winner board `shouldBe` Wins marker
    it (printf "detects wins on col %s for %s" (show winIndex) (show marker)) do
      let board =
            MkMatrix \_ j ->
              if j == winIndex then Just marker else Nothing
      winner board `shouldBe` Wins marker
  for_ [X, O] \marker -> do
    it (printf "detects wins on the diagonal for %s" (show marker)) do
      winner (diagonalMatrix marker) `shouldBe` Wins marker
    it (printf "detects wins on the other diagonal for %s" (show marker)) do
      winner (otherDiagonalMatrix marker) `shouldBe` Wins marker

emptySpotsSpec :: Spec
emptySpotsSpec = describe "emptySpotsSpec" do
  prop "correctness via lists" \m ->
    sort (emptySpots m)
    `shouldBe`
      sort
        (mapMaybe
          (\(i, j, x) -> if isNothing x then Just (i, j) else Nothing)
          (matrixToiList m))
