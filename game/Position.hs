module Position where

data Player
  = PlayerX
  | PlayerO
  deriving (Show, Eq)

class Position position move params | position -> move, position -> params where
  initialPosition :: params -> position
  makeMove :: position -> Player -> move -> Maybe position
  checkWinner :: position -> Maybe Player
  generateMoves :: position -> [move]
