module ClassicPosition where

import Data.Foldable
import Data.Maybe

import Matrix
import Position

newtype ClassicPosition = ClassicPosition
  { getMatrix :: Matrix Cell
  } deriving (Show)

type ClassicMove = Coord

type Cell = Maybe Player

data ClassicParams =
  ClassicParams Int Int
  deriving (Show)

data Triple a =
  Triple a a a
  deriving (Show)

instance Position ClassicPosition ClassicMove ClassicParams where
  initialPosition (ClassicParams sx sy) = ClassicPosition $ newMatrix (Coord sx sy) Nothing
  checkWinner p = asum $ map isTripleWin $ getTriples p
  makeMove (ClassicPosition m) player c =
    case getAt m c of
      Nothing -> Just $ ClassicPosition $ setAt m c $ Just player
      _ -> Nothing
  generateMoves (ClassicPosition m) =
    [c | x <- [0 .. sizeX m - 1], y <- [0 .. sizeY m - 1], let c = Coord x y, isNothing $ getAt m c]

isTripleWin :: Triple Cell -> Maybe Player
isTripleWin (Triple a b c) =
  if a == b && b == c
    then a
    else Nothing

getTriples :: ClassicPosition -> [Triple Cell]
getTriples (ClassicPosition m) = rows ++ cols ++ diags ++ revDs
  where
    sx = sizeX m
    sy = sizeY m
    rows =
      [ Triple (getAt m (Coord x y)) (getAt m (Coord (x + 1) y)) (getAt m (Coord (x + 2) y))
      | x <- [0 .. sx - 3]
      , y <- [0 .. sy - 1]
      ]
    cols =
      [ Triple (getAt m (Coord x y)) (getAt m (Coord x (y + 1))) (getAt m (Coord x (y + 2)))
      | x <- [0 .. sx - 1]
      , y <- [0 .. sy - 3]
      ]
    diags =
      [ Triple
        (getAt m (Coord x y))
        (getAt m (Coord (x + 1) (y + 1)))
        (getAt m (Coord (x + 2) (y + 2)))
      | x <- [0 .. sx - 3]
      , y <- [0 .. sy - 3]
      ]
    revDs =
      [ Triple
        (getAt m (Coord x y))
        (getAt m (Coord (x - 1) (y + 1)))
        (getAt m (Coord (x - 2) (y + 2)))
      | x <- [2 .. sx - 1]
      , y <- [0 .. sy - 3]
      ]
