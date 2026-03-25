module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import MatrixTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [matrixTests]
