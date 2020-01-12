module Data.Stream where

import qualified Prelude as P
import Data.Function (fix)

data Stream a = a :& Stream a

infixr 5 :&

instance P.Show a => P.Show (Stream a) where
  show xs = showInfinity (P.show (take 3 xs))
    where
      showInfinity x = P.init x P.++ "..."

constStream :: a -> Stream a
constStream x = fix (x :&)

head :: Stream a -> a
head (x :& xs) = x

tail :: Stream a -> Stream a
tail (x :& xs) = xs

(!!) :: Stream a -> P.Int -> a
(!!) (x :& xs) 0 = x
(!!) (x :& xs) i = xs !! (i P.- 1)

take :: P.Int -> Stream a -> [a]
take 0 _ = []
take i (x :& xs) = x : take (i P.- 1) xs

map :: (a -> b) -> Stream a -> Stream b
map f (x :& xs) = f x :& map f xs

filter :: (a -> P.Bool) -> Stream a -> Stream a
filter p (x :& xs) = if p x then x :& filter p xs else filter p xs

zip :: Stream a -> Stream b -> Stream (a, b)
zip = zipWith (,)

zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f (x :& xs) (y :& ys) = f x y :& zipWith f xs ys

iterate :: (a -> a) -> a -> Stream a
iterate f x = x :& iterate f (f x)
