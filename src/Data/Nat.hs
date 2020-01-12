module Data.Nat where

data Nat = Zerro | Succ Nat
  deriving (Show, Eq, Ord)

emptyNat = Zerro

singletonNat = Succ Zerro

instance Num Nat where
  (+) = foldNat id (Succ .)
  negate _ = error "negate undefined for Nat"
  (*) a Zerro = Zerro
  (*) a (Succ b) = a + (a * b)
  abs x = x
  signum Zerro = Zerro
  signum _ = Succ Zerro
  fromInteger 0 = Zerro
  fromInteger n = Succ (fromInteger (n-1))

foldNat :: a -> (a -> a) -> Nat -> a
foldNat zero succ Zerro = zero
foldNat zero succ (Succ nat) = succ (foldNat zero succ nat)

beside :: Nat -> Nat -> Bool
beside x y = left == y || x == right
  where (Succ left) = x
        (Succ right) = y
