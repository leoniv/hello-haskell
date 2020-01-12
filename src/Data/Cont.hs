module Data.Cont where

newtype Cont r a = Cont { runCont :: (a -> r) -> r}

instance Functor (Cont r) where
  fmap f xf = Cont $ \r -> runCont xf (r . f)

--instance Applicative (Cont r) where
--  pure a = Cont $ \r -> r a
--  f <*> xf = Cont $ \r ->
--    let g = runCont f r
--    in runCont xf g

square x = x * x

inc x = x + 1

squareCc = \r -> r

