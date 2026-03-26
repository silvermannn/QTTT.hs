module Matrix where

import qualified Data.Vector as V

data Matrix a = Matrix
  { sizeX :: Int
  , sizeY :: Int
  , vector :: V.Vector a
  } deriving (Show)

data Coord =
  Coord Int Int
  deriving (Show, Eq, Ord)

validCoord :: Coord -> Coord -> Bool
validCoord (Coord mx my) (Coord x y) = x < mx && y < my

newMatrix :: Coord -> a -> Matrix a
newMatrix (Coord rs cs) a = Matrix rs cs $ V.replicate (rs * cs) a

coordsToIndex :: Matrix a -> Coord -> Int
coordsToIndex m (Coord x y) = x + y * sizeX m

setAt :: Matrix a -> Coord -> a -> Matrix a
setAt m c a = m {vector = vector m V.// [(coordsToIndex m c, a)]}

getAt :: Matrix a -> Coord -> a
getAt m c = vector m V.! coordsToIndex m c
