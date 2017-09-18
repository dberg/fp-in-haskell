module Option
  ( Option(None, Some)
  ) where

import List

data Option a = None | Some a

instance (Show a) => Show (Option a) where
  show None = "None"
  show (Some a) = "Some(" ++ (show a) ++ ")"

instance (Eq a) => Eq (Option a) where
  None == None = True
  (Some a) == (Some b) = a == b
  _ == _ = False

-- Exercise 4.1
mapO :: Option a -> (a -> b) -> Option b
mapO None _ = None
mapO (Some a) f = Some $ f a

getOrElse :: Option a -> a -> a
getOrElse None a = a
getOrElse (Some a) _ = a

flatMapO :: Option a -> (a -> Option b) -> Option b
flatMapO o f = getOrElse (mapO o f) None

orElse :: Option a -> Option a -> Option a
orElse None o = o
orElse o _ = o

filterOp :: Option a -> (a -> Bool) -> Option a
filterOp o f = flatMapO o (\ a -> if f a then o else None)

-- Exercise 4.2
-- TODO:

-- Textbook
liftOp :: (a -> b) -> (Option a -> Option b)
liftOp f = \ oa -> mapO oa f

-- Exercise 4.3
mapO2 :: Option a -> Option b -> (a -> b -> c) -> Option c
mapO2 oa ob f = flatMapO oa (\ a -> mapO ob (\ b -> f a b))

-- Exercise 4.4
sequenceO :: List (Option a) -> Option (List a)
sequenceO Nil = Some Nil
sequenceO (Cons h t) = flatMapO h (\ hh -> mapO (sequenceO t) (\ tt -> Cons hh tt))

-- Exercise 4.5
traverseO :: List a -> (a -> Option b) -> Option (List b)
-- straightforward using mapL and sequenceO
--traverseO l f = sequenceO (mapL l f)

-- similar to sequenceO
traverseO Nil _ = Some Nil
traverseO (Cons h t) f = flatMapO (traverseO t f)
  (\ bt -> mapO (f h) (\ bh -> Cons bh bt))

-- via mapO2
-- TODO:
