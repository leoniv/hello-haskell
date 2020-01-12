module Data.Road where

data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)
type RoadSystem = [Section]
data Lable = A | B | C deriving (Show, Eq)
type Path = [(Lable, Int)]

heathrowLondon = [ Section 50 10 30
                 , Section 5 90 20
                 , Section 40 2 25
                 , Section 10 8 0
                 ]

pathTime :: Path -> Int
pathTime path =
  sum $ map snd path

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
  let timeA = pathTime pathA
      timeB = pathTime pathB
      forwardTimeToA = timeA + a
      forwardTimeToB = timeB + b
      crossTimeToA = forwardTimeToB + c
      crossTimeToB = forwardTimeToA + c
      pathToA = if forwardTimeToA <= crossTimeToA
                   then (A, a):pathA
                   else (C, c):(B, b):pathB
      pathToB = if forwardTimeToB <= crossTimeToB
                   then (B, b):pathB
                   else (C, c):(A, a):pathA
      in (pathToA, pathToB)

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
  let (pathA, pathB) = foldl roadStep ([], []) roadSystem
   in if pathTime pathA <= pathTime pathB
      then reverse pathA
      else reverse pathB
