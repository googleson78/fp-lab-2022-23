{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module TUI where

import Control.Applicative (liftA2)
import Game qualified
import Matrix (Three (..), showMatrix)

data InputParseError = InputWrongSize | ThreeNoParse Char

loop :: Game.State -> IO ()
loop state = do
  putStrLn $ prettyState state
  inputLine <- getLine
  case parseInputLine inputLine of
    Left err -> do
      putStrLn $ prettyInputParseError err
      loop state
    Right input -> do
      case Game.act input state of
        Left err -> do
          putStrLn $ prettyGameError err
          loop state
        Right actResult -> do
          case actResult of
            Game.GameEnded gameEndResult -> do
              putStrLn $ prettyGameEndResult gameEndResult
              loop $ Game.initialState $ Game.flipMarker $ Game.humanPlayer state
            Game.StillGoing nextState -> loop nextState

prettyState :: Game.State -> String
prettyState Game.MkState {Game.board} = showMatrix showSpot board
  where
    showSpot Nothing = "_"
    showSpot (Just marker) = show marker

prettyInputParseError :: InputParseError -> String
prettyInputParseError InputWrongSize = "The input needs to be exactly two characters long."
prettyInputParseError (ThreeNoParse c) = show c ++ " does not parse as a Marker. Chars needs to be one of X or O."

prettyGameError :: Game.Error -> String
prettyGameError Game.SpotTaken = "That spot is taken. Try another one."
prettyGameError Game.OutOfBounds = "That spot is is out of bounds. Try another one."

parseInputLine :: String -> Either InputParseError Game.Input
parseInputLine [i, j] =
  liftA2 Game.Place (parseThree i) (parseThree j)
parseInputLine _ = Left InputWrongSize

parseThree :: Char -> Either InputParseError Three
parseThree = \case
  '0' -> Right Zero
  '1' -> Right One
  '2' -> Right Two
  c -> Left $ ThreeNoParse c

prettyGameEndResult :: Game.GameEndResult -> String
prettyGameEndResult Game.GameDrawn = "Draw!"
prettyGameEndResult (Game.Winner m) = show m ++ " is a winrar!"
