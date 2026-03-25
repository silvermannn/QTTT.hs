module Position where

data Player
  = PlayerX
  | PlayerO
  deriving (Show, Eq)

data Cell
  = Empty
  | Occupied Player
  deriving (Show)

class Position p m | p -> m where
  initialPosition :: p
  makeMove :: a -> Player -> m -> a
  checkWinner :: a -> Maybe Player
