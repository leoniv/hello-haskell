{-# LANGUAGE DeriveFunctor #-}

module Data.MonadExplore where

import Control.Monad.State
import Control.Monad.Writer
import Data.Bifunctor (first, second)
import Data.Ratio

data Maybe' a = Nothing' | Just' a deriving (Show, Functor)

instance Applicative Maybe' where
  pure = Just'
  Nothing' <*> _ = Nothing'
  (Just' f) <*> (Just' a) = Just' (f a)

instance Monad Maybe' where
  Nothing' >>= _ = Nothing'
  (Just' a) >>= f = f a

main' = do
  a <- Just 3
  b <- Nothing
  return (show a ++ b)

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) =
  filter
    onBoard
    [ (c + 2, r - 1),
      (c + 2, r + 1),
      (c - 2, r - 1),
      (c - 2, r + 1),
      (c + 1, r - 2),
      (c + 1, r + 2),
      (c - 1, r - 2),
      (c - 1, r + 2)
    ]
  where
    onBoard (c, r) = c `elem` [1 .. 8] && r `elem` [1 .. 8]

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Given " ++ show x])

logAdd = do
  tell ["When"]
  x <- logNumber 10
  tell ["And"]
  y <- logNumber 12
  tell ["Then + it"]
  return (x + y)

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
  | b == 0 = do
    tell ["End " ++ show a]
    return a
  | otherwise = do
    tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
    gcd' b (a `mod` b)

addStaf :: Int -> Int
addStaf = do
  a <- (* 2)
  return (a + 10)

type Stack a = [a]

--maybePop :: [Int] -> (Maybe Int, [Int])

pop :: Num a => State (Stack a) (Maybe a)
pop = state maybePop
  where
    maybePop [] = (Nothing, [])
    maybePop (x : xs) = (Just x, xs)

--  \(x:xs) -> (Just x, xs)

push :: Num a => a -> State (Stack a) ()
push x = state $ \xs -> ((), x : xs)

newtype Prob a = Prob {getProb :: [(a, Rational)]} deriving (Show)

instance Functor Prob where
  fmap f (Prob xs) = Prob $ map (first f) xs

instance Applicative Prob where
  pure a = Prob [(a, 1)]
  Prob fs <*> Prob xs = Prob [(f a, p) | (f, _) <- fs, (a, p) <- xs]

instance Monad Prob where
  prob >>= f = flatten (fmap f prob)

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob (concatMap multAll xs)
  where
    multAll (Prob innerxs, p) = map (second (p *)) innerxs

data Coin = Tails | Heads deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads, 1 % 2), (Tails, 1 % 2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads, 1 % 10), (Tails, 9 % 10)]

flipThree :: Prob [Coin]
flipThree = do
  -- a <- coin
  b <- coin
  c <- loadedCoin
  return [b, c]

foo :: Maybe String
foo = do
  x <- Just 3
  y <- Just "!"
  Just (show x ++ y)
