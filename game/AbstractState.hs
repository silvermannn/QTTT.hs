module AbstractState where

data Player
  = PlayerX
  | PlayerO
  deriving (Show, Eq)

data Point =
  Point
  deriving (Show, Eq)

class AbstractState a where
  checkWinner :: a -> Maybe Player
  makeMove :: a -> Player -> Point -> a
