module Main where

import List
import Tree

main :: IO ()
main = putStrLn (show (setHead (tail' (build ["Bogus", "Hallo", "World"])) "Hello"))
