module Data.Category where

import Prelude hiding (id, (>>), (*>), pred)
import Data.Nat

class Category cat where
    id   :: cat a a
    (>>) :: cat a b -> cat b c -> cat a c

class Kleisli m where
    idK  :: a -> m a
    (*>) :: (a -> m b) -> (b -> m c) -> (a -> m c)

(+>) :: Kleisli m => (a -> m b) -> (b -> c) -> (a -> m c)
f +> g = f *> (g >> idK)

(*$) :: Kleisli m => (a -> m b) -> m a -> m b
f *$ a = (const a *> f) ()
(+$) :: Kleisli m => (a -> b)   -> m a -> m b
f +$ a = (const a +> f) ()
infixr 0 +$, *$

($$) :: Kleisli m => m (a -> b) -> m a -> m b
mf $$ ma = ( +$ ma) *$ mf
-- Экземпляр для функций

instance Category (->) where
    id      = \x -> x
    f >> g  = \x -> g (f x)

instance Kleisli Maybe where
  idK = Just
  f *> g = \a -> case f a of
                   Nothing -> Nothing
                   Just b -> g b

nat10 = fromInteger 10 :: Nat

pred :: Nat -> Maybe Nat
pred Zerro = Nothing
pred (Succ a) = Just a

pred' :: Int -> Nat -> Maybe Nat
pred' 0 n = Just n
pred' _ Zerro = Nothing
pred' n (Succ a) = pred' (n - 1) a

pred'' :: Int -> Nat -> Maybe Nat
pred'' n = foldl (*>) idK (replicate n pred)

instance Kleisli [] where
  idK x = [x]
  -- f *> g = f >> map g >> concat
  f *> g = concat . map g . f

next :: Char -> String
next 'a' = "ab"
next 'b' = "a"

generate :: Int -> (a -> [a]) -> (a -> [a])
generate 0 f = idK
generate n f = f *> generate (n-1) f
