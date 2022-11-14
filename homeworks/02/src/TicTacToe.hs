module TicTacToe where

import Matrix

data Marker = X | O
  deriving (Eq, Show)

type Spot = Maybe Marker

type Board = Matrix Spot

data Result = Full | HasEmpty | Wins Marker
  deriving (Show)

infixr 2 `join`

join :: Result -> Result -> Result
join = error "not implemented"

checkThreeSpots :: Thrice Spot -> Result
checkThreeSpots = error "not implemented"

winnerRows :: Board -> Result
winnerRows = error "not implemented"

winnerCols :: Board -> Result
winnerCols = error "not implemented"

winnerDiags :: Board -> Result
winnerDiags = error "not implemented"

winner :: Board -> Result
winner = error "not implemented"

emptySpots :: Board -> [(Three, Three)]
emptySpots = error "not implemented"
