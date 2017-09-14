module Option
  ( Option(None, Some)
  ) where

data Option a = None | Some a

instance (Show a) => Show (Option a) where
  show None = "None"
  show (Some a) = "Some(" ++ (show a) ++ ")"

instance (Eq a) => Eq (Option a) where
  None == None = True
  (Some a) == (Some b) = a == b
  _ == _ = False

-- Exercise 4.1
mapOp :: Option a -> (a -> b) -> Option b
mapOp None _ = None
mapOp (Some a) f = Some $ f a

getOrElse :: Option a -> a -> a
getOrElse None a = a
getOrElse (Some a) _ = a

flatMapO :: Option a -> (a -> Option b) -> Option b
flatMapO o f = getOrElse (mapOp o f) None

orElse :: Option a -> Option a -> Option a
orElse None o = o
orElse o _ = o

filterOp :: Option a -> (a -> Bool) -> Option a
filterOp o f = flatMapO o (\ a -> if f a then o else None)

-- Exercise 4.2
-- TODO:
