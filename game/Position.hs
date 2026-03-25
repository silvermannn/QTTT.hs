module Position where

data Player
  = PlayerX
  | PlayerO
  deriving (Show, Eq)

class Position p m | p -> m where
  initialPosition :: p
  makeMove :: p -> Player -> m -> Maybe p
  checkWinner :: p -> Maybe Player
  generateMoves :: p -> [m]
