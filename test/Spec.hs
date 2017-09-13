module Main where

import Test.Tasty
import Test.Tasty.HUnit

import List
import Tree

main :: IO ()
main = do
  defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTestLists, unitTestTrees]

unitTestLists = testGroup "List tests"
  [ testCase "Build empty list" $
      assertEqual "Nil" (build [] :: List Int) (Nil :: List Int)

  , testCase "Build list single element" $
      assertEqual "[1]" (build [1]) (Cons 1 Nil)

  , testCase "Build list with a few elements" $
      assertEqual "[1, 2, 3]" (build [1, 2, 3]) (Cons 1 (Cons 2 (Cons 3 Nil)))

  , testCase "Filter odd elements" $
      assertEqual "[2]" (filter' (build [1, 2, 3]) (\ a -> a `mod` 2 == 0)) (build [2])

  , testCase "Concat lists" $
      assertEqual "[1, 2, 3]" (concat' $ build [build [1], build [2], build [3]]) (build [1, 2, 3])

  , testCase "Concat lists with empty elements" $
      assertEqual "[1, 2]" (concat' $ build [build [1], build [], build [2]]) (build [1, 2])

  , testCase "Flatmap" $
      assertEqual "[1, 1, 2, 2]" (flatMap (build [1, 2]) (\ i -> build [i, i])) (build [1, 1, 2, 2])
  ]

unitTestTrees = testGroup "Tree tests"
  [ testCase "count nodes" $
      assertEqual "size 3" (size (Branch (Leaf 1) (Leaf 2))) 3

  , testCase "maximum int" $
      assertEqual "max 10" (maximum' (Branch (Branch (Leaf 10) (Leaf 5)) (Leaf 15))) 15

  , testCase "max depth 0" $
      assertEqual "max 0" (depth (Leaf 10)) 0

  , testCase "max depth 3" $
      assertEqual "max 3" (depth (Branch (Branch (Leaf 3) (Leaf 2)) (Leaf 1))) 2

  , testCase "map add 1" $
      assertEqual "map"
      (map' (Branch (Leaf 1) (Leaf 2)) (\ a -> a + 1))
      (Branch (Leaf 2) (Leaf 3))
  ]
