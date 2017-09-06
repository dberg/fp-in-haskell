module Lib
    ( List(Cons, Nil), build, tail', setHead, drop', tail'', dropWhile', init', foldRight, length', foldLeft
    ) where

data List a = Nil | Cons a (List a)

instance (Show a) => Show (List a) where
  show (Nil) = ""
  show (Cons x xs) = show x ++ " " ++ show xs

-- TODO: Check how Varargs can be implemented. Also, see [] implementation.
--       https://wiki.haskell.org/Varargs
build :: [a] -> List a
build [] = Nil
build (h:t) = Cons h (build t)

-- Exercise 3.2
tail' :: List a -> List a
tail' Nil = Nil
tail' (Cons _ t) = t

-- Exercise 3.3
setHead :: List a -> a -> List a
setHead Nil h = build [h]
setHead (Cons _ t) h = Cons h t

-- Exercise 3.4
drop' :: List a -> Int -> List a
drop' Nil _ = Nil
drop' (Cons _ t) 0 = t
drop' (Cons _ t) n = drop' t (n - 1)

tail'' :: List a -> List a
tail'' l = drop' l 1

-- Exercise 3.5
dropWhile' :: List a -> (a -> Bool) -> List a
dropWhile' Nil _ = Nil
dropWhile' l @ (Cons h t) p = if p h then dropWhile' t p else l

-- Exercise 3.6
init' :: List a -> List a
init' Nil = Nil
init' (Cons h Nil) = Cons h Nil
init' (Cons h t) = Cons h (init' t)

-- Textbook
foldRight :: List a -> b -> (a -> b -> b) -> b
foldRight Nil z _ = z
foldRight (Cons h t) z f = f h (foldRight t z f)

sumInts :: List Int -> Int
sumInts l = foldRight l 0 (\ a b -> a + b)

productDoubles :: List Double -> Double
productDoubles l = foldRight l 0.0 (\ a b -> a * b)

-- Exercise 3.7
-- Can product short-circuit if 0.0 is found? Nope

-- Exercise 3.8
-- What happens? foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_, _))
-- Same List is built and returned.

-- Exercise 3.9
length' :: List a -> Int
length' l = foldRight l 0 (\ _ b -> b + 1)

-- Exercise 3.10
foldLeft :: List a -> b -> (b -> a -> b) -> b
foldLeft Nil z _ = z
foldLeft (Cons h t) z f = foldLeft t (f z h) f

-- Exercise 3.11
-- TODO: sum, product, and length with foldLeft
sumInts' :: List Int -> Int
sumInts' l = foldLeft l 0 (\ z i -> z + i)

productDoubles' :: List Double -> Double
productDoubles' l = foldLeft l 0.0 (\ d z -> d * z)

-- Exercise 3.13
-- Can we write foldLeft in terms of foldRight?
-- What about the other way around.

-- Exercise 3.14
-- Implement append in terms of either foldLeft or foldRight

-- Exercise 3.15
-- Function that concatenates a list of list into a single list.
-- Runtime should be linear in the total length of all lists.
-- Use functions we have already defined.
