{-# LANGUAGE NamedFieldPuns #-}

module Game where

import Control.Monad (unless, when)
import Matrix
import MinMax.TicTacToe (optimiseFor)
import TicTacToe

data State = MkState
  { board :: Board,
    humanPlayer :: Marker
  }

initialState :: Marker -> State
initialState humanPlayer =
  MkState
    { board = case humanPlayer of
        X -> constantMatrix Nothing
        O -> computerPlay humanPlayer emptyMatrix,
      humanPlayer
    }
  where
    emptyMatrix :: Matrix (Maybe a)
    emptyMatrix = constantMatrix Nothing

data Input = Place Three Three

data ActResult = GameEnded GameEndResult | StillGoing State

data GameEndResult = GameDrawn | Winner Marker

data Error = SpotTaken | OutOfBounds

act :: Input -> State -> Either Error ActResult
act (Place i j) MkState {board = currentBoard, humanPlayer} = do
  let empties = emptySpots currentBoard

  when (null empties) $ error "assert: played on a full board"
  when (outOfBounds i || outOfBounds j) $ Left OutOfBounds
  unless ((i, j) `elem` empties) $ Left SpotTaken

  let playerPlayedBoard = place i j (Just humanPlayer) currentBoard
  Right $
    whenNothing (hasGameEnded playerPlayedBoard) $
      let computerPlayedBoard = computerPlay humanPlayer playerPlayedBoard
       in whenNothing (hasGameEnded computerPlayedBoard) $
            StillGoing $
              MkState {board = computerPlayedBoard, humanPlayer}
  where
    hasGameEnded :: Board -> Maybe ActResult
    hasGameEnded board =
      case winner board of
        Full -> Just $ GameEnded GameDrawn
        Wins m -> Just $ GameEnded $ Winner m
        HasEmpty -> Nothing

    whenNothing :: Maybe c -> c -> c
    whenNothing Nothing x = x
    whenNothing (Just y) _ = y

flipMarker :: Marker -> Marker
flipMarker X = O
flipMarker O = X

computerPlay :: Marker -> Matrix Spot -> Matrix Spot
computerPlay humanPlayer board =
  let (oi, oj) = optimiseFor board
   in place oi oj (Just $ flipMarker humanPlayer) board

outOfBounds :: Three -> Bool
outOfBounds = (`notElem` [Zero, One, Two])
