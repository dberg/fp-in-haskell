-- Also known as Either ;)
module OneOfTwo
  ( OneOfTwo(Fst, Snd)
  ) where

data OneOfTwo a b = Fst a | Snd b

instance (Show a, Show b) => Show (OneOfTwo a b) where
  show (Fst a) = "Fst " ++ (show a)
  show (Snd b) = "Snd " ++ (show b)

instance (Eq a, Eq b) => Eq (OneOfTwo a b) where
  (Fst x) == (Fst y) = x == y
  (Snd x) == (Snd y) = x == y
  _ == _ = False

-- Exercise 4.6
mapE :: (OneOfTwo a b) -> (b -> c) -> OneOfTwo a c
mapE (Fst a) _ = Fst a
mapE (Snd b) f = Snd $ f b

flatMapE :: (OneOfTwo a b) -> (b -> (OneOfTwo a c)) -> OneOfTwo a c
flatMapE (Fst a) _ = Fst a
flatMapE (Snd b) f = f b

orElseE :: (OneOfTwo a b) -> (OneOfTwo c b) -> OneOfTwo c b
orElseE (Fst _) r = r
orElseE (Snd x) _ = Snd x

map2E :: (OneOfTwo a b) -> (OneOfTwo a c) -> (b -> c -> d) -> (OneOfTwo a d)
map2E (Snd b) (Snd c) f = Snd (f b c)
map2E (Fst x) _ _ = Fst x
map2E (Snd _) (Fst y) _ = Fst y
