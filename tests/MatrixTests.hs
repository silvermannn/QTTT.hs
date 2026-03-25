module MatrixTests where

import Test.Tasty
import Test.Tasty.QuickCheck

import Matrix

matrixTests :: TestTree
matrixTests =
  testGroup
    "Matrix tests"
    [ testProperty "new matrix data" checkNewMatrixData
    , testProperty "set matrix data" checkSetMatrixData
    ]

instance Arbitrary Coord where
  arbitrary = Coord <$> choose (0, 100) <*> choose (0, 100)

checkNewMatrixData :: Coord -> Coord -> Int -> Property
checkNewMatrixData sz c a = validCoord sz c ==> getAt (newMatrix sz a) c === a

checkSetMatrixData :: Coord -> Coord -> Int -> Property
checkSetMatrixData sz c a = validCoord sz c ==> getAt (setAt (newMatrix sz a) c a) c === a
