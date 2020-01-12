module Data.Free where

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)

instance Applicative Tree where
  pure = Leaf
  Leaf f <*> a = fmap f a
  Node l r <*> a = Node (l <*> a) (r <*> a)

instance Monad Tree where
  return = Leaf
  (>>=) = subst


subst :: Tree a -> (a -> Tree b) -> Tree b
subst (Leaf a) f = f a
subst (Node l r) f = Node (subst l f) (subst r f)

fullTree :: Int -> Tree Int
fullTree 1 = Leaf 1
fullTree n = subst (fullTree (n - 1)) last
  where
    last i = Node (return (n - i - 1)) (return (i + 1))
--fullTree n = do
--  i <- fullTree (n - 1)
--  Node (Leaf (n - i -1)) (Leaf (i + 1))

zigzag :: Tree a -> a
zigzag = zig where
  zig (Leaf x) = x
  zig (Node l r) = zag l
  zag (Leaf x) = x
  zag (Node l r) = zig r

tree = Node (Leaf 1) (Leaf 2)

newtype CTree b a = CTree ((a -> Tree b) -> Tree b)

rep :: Tree a -> CTree b a
rep t = CTree (t >>=)

abs' :: CTree a a -> Tree a
abs' (CTree p) = p Leaf

--instance Monad CTree where
--  return a = CTree (\h -> h a)
--  CTree p >>= k =
