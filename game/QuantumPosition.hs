module QuantumPosition where

import ClassicPosition
import Matrix
import Position

newtype QuantumPosition =
  QuantumPosition [ClassicPosition]
  deriving (Show)

data QuantumMove
  = SingleMove Coord
  | EntangledMove Coord Coord
  deriving (Show)

instance Position QuantumPosition QuantumMove where
  initialPosition = QuantumPosition [initialPosition]
  checkWinner p = undefined
  makeMove p player c = undefined
  generateMoves p = undefined
