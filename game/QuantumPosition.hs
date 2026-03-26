module QuantumPosition where

import System.Random

import Data.Foldable
import Data.List
import Data.Maybe

import ClassicPosition
import Matrix
import Position

data QuantumPosition = QuantumPosition
  { boardSizeX :: Int
  , boardSizeY :: Int
  , classicPositions :: [ClassicPosition]
  , rnd :: StdGen
  } deriving (Show)

data QuantumMove
  = ClassicMove Coord
  | QuantumMove Coord Coord
  deriving (Show)

data QuantumParams =
  QuantumParams Int Int Int
  deriving (Show)

instance Position QuantumPosition QuantumMove QuantumParams where
  initialPosition (QuantumParams sx sy seed) =
    QuantumPosition sx sy [initialPosition (ClassicParams sx sy)] (mkStdGen seed)
  checkWinner p = asum $ checkWinner <$> classicPositions p
  makeMove p player (ClassicMove c) = makeQuantumMove p player [c]
  makeMove _ _ (QuantumMove c1 c2)
    | c1 == c2 = Nothing
  makeMove p player (QuantumMove c1 c2) = makeQuantumMove p player [c1, c2]
  generateMoves p =
    fmap ClassicMove moves ++ [QuantumMove c1 c2 | c1 <- moves, c2 <- moves, c1 < c2]
    where
      sx = boardSizeX p
      sy = boardSizeY p
      moves = [c | x <- [0 .. sx - 1], y <- [0 .. sy - 1], let c = Coord x y, isAny p c isNothing]

isAny :: QuantumPosition -> Coord -> (Cell -> Bool) -> Bool
isAny p c cond = any (cond . (`getAt` c) . getMatrix) (classicPositions p)

makeQuantumMove :: QuantumPosition -> Player -> [Coord] -> Maybe QuantumPosition
makeQuantumMove p player cs =
  case partition (cond . getMatrix) (classicPositions p) of
    -- Nowhere to place
    ([], _) -> Nothing
    -- Absolutely empty cell
    (empties, []) -> Just $ p {classicPositions = newPositions empties}
    -- Partially empty cell, decoherence occures
    (empties, occupied) ->
      Just
        $ p
            { classicPositions =
                if canPlace
                  then newPositions empties
                  else occupied
            , rnd = rnd'
            }
      where
        (canPlace, rnd') = uniform $ rnd p
  where
    cond m = and [isNothing (getAt m c) | c <- cs]
    newPositions empties = concat [fmap (\p' -> fromJust $ makeMove p' player c) empties | c <- cs]
