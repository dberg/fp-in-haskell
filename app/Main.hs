module Main where

import Lib

main :: IO ()
main = putStrLn (show (setHead (tail' (build ["Bogus", "Hallo", "World"])) "Hello"))
