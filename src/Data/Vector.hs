module Data.Vector
(
  Vector (..)
, vplus
, scalarProd
, vmult
)
where

data Vector a = Vector a a a deriving (Show, Eq)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

scalarProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `scalarProd` (Vector l m n) = i * l + j * m + k * n

vmult :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vmult` (Vector l m n) = Vector (i * l) (j * m) (k * n)
