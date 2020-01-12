module Data.Rand where

import System.Random
import Control.Monad.State

threeCoins :: State StdGen (Int, Float, Bool)
threeCoins = do
  a <- randStr
  b <- randStr
  c <- randStr
  return (a, b, c)

randStr :: (RandomGen g, Random a) => State g a
randStr = state random
