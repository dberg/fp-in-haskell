module List
    ( List(Cons, Nil)
    , build
    , tail'
    , setHead
    , drop'
    , tail''
    , dropWhile'
    , init'
    , foldRight
    , length'
    , foldLeft
    , filter'
    , concat'
    , flatMap
    ) where

data List a = Nil | Cons a (List a)

instance (Show a) => Show (List a) where
  show (Nil) = ""
  show (Cons x xs) = show x ++ " " ++ show xs

instance (Eq a) => Eq (List a) where
  Nil == Nil = True
  (Cons h1 t1) == (Cons h2 t2) = h1 == h2 && t1 == t2
  _ == _ = False

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
foldLeft' :: List a -> b -> (b -> a -> b) -> b
foldLeft' l z f = foldRight l (\ b -> b) (\ a g -> (\ b -> g (f b a))) z

-- Exercise 3.14
-- Implement append in terms of either foldLeft or foldRight
append' :: List a -> List a -> List a
append' x y = foldRight x y (\ a b -> Cons a b)

-- Exercise 3.15
concat' :: List (List a) -> List a
concat' l = foldRight l Nil (\ a b -> append' a b)

-- Exercise 3.16
addOne :: List Int -> List Int
addOne Nil = Nil
addOne (Cons h t) = Cons (h + 1) (addOne t)

-- Exercise 3.17
doubleToString :: List Double -> List String
doubleToString Nil = Nil
doubleToString (Cons h t) = Cons (show h) (doubleToString t)

-- Exercise 3.18
map' :: List a -> (a -> b) -> List b
map' Nil _ = Nil
map' (Cons h t) f = Cons (f h) (map' t f)

-- Exercise 3.19
filter' :: List a -> (a -> Bool) -> List a
filter' l f = foldRight l Nil (\ a b -> if f a then Cons a b else b)

-- Exercise 3.20
flatMap :: List a -> (a -> List b) -> List b
flatMap l f = concat' (map' l f)

-- Exercise 3.21
filter'' :: List a -> (a -> Bool) -> List a
filter'' l f = flatMap l (\ a -> if f a then build [ a ] else Nil)

-- Exercise 3.22
addTwoLists :: (Num a) => List a -> List a -> List a
addTwoLists Nil _ = Nil
addTwoLists _ Nil = Nil
addTwoLists (Cons h1 t1) (Cons h2 t2) = Cons (h1 + h2) (addTwoLists t1 t2)

-- Exercise 3.23
zipWith' :: List a -> List b -> (a -> b -> c) -> List c
zipWith' Nil _ _ = Nil
zipWith' _ Nil _ = Nil
zipWith' (Cons h1 t1) (Cons h2 t2) f = Cons (f h1 h2) (zipWith' t1 t2 f)
