module Data.Kleisli where

import Prelude hiding ( (*>))

newtype State s a = State { runState :: s -> (a, s) }
newtype Reader env b = Reader { runReader :: env -> b }
newtype Writer msg b = Writer { runWriter :: (b, msg) }

class Kleisli m where
  idK :: a -> m a
  (*>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
  (+>) :: (a -> m b) -> (b -> c) -> (a -> m c)
  (+$) :: (a -> b) -> m a -> m b
  (*$) :: (a -> m b) -> m a -> m b
  ($$) :: m (a -> b) -> m a -> m b
  (*>>) :: m a -> m b -> m b
  infixr 0 +$, *$, $$
  infixl 1 *>>
  f +> g = f *> (idK . g)
  f +$ ma = (const ma +> f)()
  f *$ ma = (const ma *> f)()
  mf $$ ma = (+$ ma) *$ mf
  ma *>> mb = const mb *$ ma


fmap' :: Kleisli m => (a -> b) -> m a -> m b
fmap' = (+$)
(==<<) :: Kleisli m => (a -> m b) -> m a -> m b
(==<<) = (*$)
(>>==) :: Kleisli m => m a -> (a -> m b) -> m b
(>>==) = flip (==<<)

instance Kleisli Maybe where
  idK = Just
  f *> g = \x -> case f x of
                   Nothing -> Nothing
                   (Just a) -> g a

instance Kleisli [] where
  idK a = [a]
  f *> g = concatMap g . f

push :: a -> State [a] ()
push a = State (\s -> ((), a:s))

pop :: State [a] a
pop = State (\(a:st) -> (a, st))


instance Kleisli (State s) where
  idK a = State (\s -> (a, s))
  f *> g = \a -> State $ \s -> let (b, st) = runState (f a) s in runState (g b) st

instance Kleisli (Reader env) where
  idK a = Reader (\env -> a)
  f *> g = \a -> Reader $ \env -> let b = runReader (f a) env in runReader (g b) env

instance Monoid msg => Kleisli (Writer msg) where
  idK a = Writer (a, mempty)
  f *> g = \a -> Writer $ let (b, msga) = runWriter (f a)
                              (c, msgb) = runWriter (g b) in
                              (c, msga `mappend` msgb)

write' :: (Show a, Num a) => a -> Writer [String] a
write' a = Writer (a + 1, ["log <" ++ (show a) ++ ">"])

read' :: Int -> Reader [a] a
read' i = Reader (\env -> (env !! i))

collect' :: [a] -> Reader [a] [a]
collect' xs = Reader (\(a:env) -> a:xs)

liftM1 :: Kleisli m => (a -> b) -> m a -> m b
liftM1 = (+$)

liftM2 :: Kleisli m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f a b = liftM1 f a $$ b
