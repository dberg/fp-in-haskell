module Main where

import Lib

main :: IO ()
main = putStrLn (show (Cons "Hello" Nil))
