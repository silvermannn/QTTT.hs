module ClassicPosition where

import Matrix
import Position

newtype ClassicPosition =
  ClassicPosition (Matrix Cell)
  deriving (Show)

newtype ClassicMove =
  ClassicMove Coord
  deriving (Show)

instance Position ClassicPosition ClassicMove where
  initialPosition = ClassicPosition $ newMatrix (Coord 3 3) Empty
  checkWinner = undefined
  makeMove = undefined

getTriples :: ClassicPosition -> [Cell]
getTriples (ClassicPosition m) = undefined
