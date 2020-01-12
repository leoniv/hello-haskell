module Data.Geometry.Types
(
  Point(..)
, Shape(..)
, baseCircle
, baseRectangel
)
where

type Radius      = Float
data Point       = Point Float Float deriving (Show, Eq)
type Center      = Point
type LeftTop     = Point
type RightBottom = Point
data Shape =
    Circle Center Radius
  | Rectangle LeftTop RightBottom deriving (Show, Eq)

baseCircle :: Radius -> Shape
baseCircle = Circle (Point 0 0)

baseRectangel :: RightBottom -> Shape
baseRectangel = Rectangle (Point 0 0)
