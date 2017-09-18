module Tree
  ( Tree(Leaf, Branch)
  , size
  , maximum'
  , depth
  , mapT
  , fold
  ) where

data Tree a = Leaf a | Branch (Tree a) (Tree a)

instance (Show a) => Show (Tree a) where
  show (Leaf a) = "(Leaf " ++ show (a) ++ ")"
  show (Branch a b) = "(Branch " ++ show a ++ " " ++ show b ++ ")"

instance (Eq a) => Eq (Tree a) where
  (Leaf a) == (Leaf b) = a == b
  (Leaf _) == (Branch _ _) = False
  (Branch _ _) == (Leaf _) = False
  (Branch a b) == (Branch c d) = a == c && b == d

-- Exercise 3.25
size :: Tree a -> Int
size (Leaf _) = 1
size (Branch t1 t2) = 1 + (size t1) + (size t2)

-- Exercise 3.26
maximum' :: Tree Int -> Int
maximum' (Leaf a) = a
maximum' (Branch t1 t2) = max (maximum' t1) (maximum' t2)

-- Exercise 3.27
depth :: Tree a -> Int
depth (Leaf _) = 0
depth (Branch t1 t2) = 1 + (max (depth t1) (depth t2))

-- Exercise 3.28
mapT :: Tree a -> (a -> b) -> Tree b
mapT (Leaf a) f = Leaf $ f a
mapT (Branch t1 t2) f = Branch (mapT t1 f) (mapT t2 f)

-- Exercise 3.29
fold :: Tree a -> (a -> b) -> (b -> b -> b) -> b
fold (Leaf a) f _ = f a
fold (Branch t1 t2) f g = g (fold t2 f g) (fold t1 f g)

-- TODO: size, maximum', depth and mapT via fold
