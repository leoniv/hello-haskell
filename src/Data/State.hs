module Data.State where

newtype State s a = State { runState :: s -> (a, s) }


instance Functor (State s) where
  fmap f sf = State $ \s ->
    let (a, newState) = runState sf s
    in (f a, newState)

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  f <*> sf = State $ \s ->
    let (g, newState) = runState f s
        (a, newNewSate) = runState sf newState
    in (g a, newNewSate)

instance Monad (State s) where
  return = pure
  sf >>= f = State $ \s ->
    let (a, newState) = runState sf s
    in runState (f a) newState
