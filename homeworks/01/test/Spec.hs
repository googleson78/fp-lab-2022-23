{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import TicTacToeSpec (ticTacToeSpec)
import MatrixSpec (matrixSpec)
import Test.Hspec (hspec)

main :: IO ()
main = hspec do
  matrixSpec
  ticTacToeSpec
