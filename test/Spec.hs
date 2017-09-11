module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Lib

main :: IO ()
main = do
  defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "Build empty list" $
      assertEqual "Nil" (build [] :: List Int) (Nil :: List Int)

  , testCase "Build list single element" $
      assertEqual "[1]" (build [1]) (Cons 1 Nil)

  , testCase "Build list with a few elements" $
      assertEqual "[1, 2, 3]" (build [1, 2, 3]) (Cons 1 (Cons 2 (Cons 3 Nil)))

  , testCase "Filter odd elements" $
      assertEqual "[2]" (filter' (build [1, 2, 3]) (\ a -> a `mod` 2 == 0)) (build [2])
  ]
