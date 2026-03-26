module QuantumPosition where

import System.Random

import Data.Foldable
import Data.List
import Data.Maybe

import Position

data QuantumPosition pos = QuantumPosition
  { positions :: [pos]
  , rnd :: StdGen
  } deriving (Show)

data QuantumMove m
  = ClassicMove m
  | QuantumMove m m
  deriving (Show)

data QuantumParams par =
  QuantumParams par Int
  deriving (Show)

instance (Position pos m par, Ord m) =>
         Position (QuantumPosition pos) (QuantumMove m) (QuantumParams par) where
  initialPosition (QuantumParams par seed) = QuantumPosition [initialPosition par] (mkStdGen seed)
  checkWinner p = asum $ checkWinner <$> positions p
  makeMove p player (ClassicMove c) = makeQuantumMove p player [c]
  makeMove _ _ (QuantumMove c1 c2)
    | c1 == c2 = Nothing
  makeMove p player (QuantumMove c1 c2) = makeQuantumMove p player [c1, c2]
  generateMoves p =
    fmap ClassicMove moves ++ [QuantumMove c1 c2 | c1 <- moves, c2 <- moves, c1 < c2]
    where
      moves = concatMap generateMoves $ positions p

makeQuantumMove ::
     Position pos move params
  => QuantumPosition pos
  -> Player
  -> [move]
  -> Maybe (QuantumPosition pos)
makeQuantumMove p player cs =
  case partition splitByEmptiness (positions p) of
    -- Nowhere to place
    ([], _) -> Nothing
    -- Absolutely empty cell
    (empties, []) -> Just $ p {positions = newPositions empties}
    -- Partially empty cell, decoherence occures
    (empties, occupied) ->
      Just
        $ p
            { positions =
                if canPlace
                  then newPositions empties
                  else occupied
            , rnd = rnd'
            }
      where
        (canPlace, rnd') = uniform $ rnd p
  where
    splitByEmptiness pos = all (isNothing . makeMove pos player) cs
    newPositions empties = concat [fmap (\p' -> fromJust $ makeMove p' player c) empties | c <- cs]
