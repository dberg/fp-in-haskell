module Lib
    ( MyList(Cons, Nil)
    ) where

data MyList a = Nil | Cons a (MyList a)

instance (Show a) => Show (MyList a) where
  show (Nil) = "Nil"
  show (Cons x xs) = show x ++ " " ++ show xs