module ClassicState where

import AbstractState

data ClassicState =
  ClassicState
  deriving (Show)

checkWinner :: ClassicState -> Maybe Player
checkWinner = undefined

makeMove :: ClassicState -> Point -> Player -> ClassicState
makeMove = undefined
