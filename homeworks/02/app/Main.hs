{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Game qualified
import TUI qualified
import TicTacToe (Marker (..))

main :: IO ()
main = TUI.loop $ Game.initialState X
