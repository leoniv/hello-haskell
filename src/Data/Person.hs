module Data.Person
(
  Person(..)
, personNew
)
where

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Show, Eq, Read)

personNew :: String -> String -> Int -> Person
personNew f l a = Person { firstName = f, lastName = l, age = a }
