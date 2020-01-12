module Data.Fixi where

--foldr' :: (a -> b -> b) -> b -> [a] -> b
--foldr' _ acc [] = acc
--foldr' f acc (x:xs) = f x (foldr f acc xs)

data St a b = St (a -> (b, St a b))

class Funct' t where
  fmap' :: (a -> b) -> t a -> t b

class Tofu t where
  tofu :: j a -> t a j

data Frank a b = Frank {frankId :: b a} deriving (Show)

instance Tofu Frank where
  tofu x = Frank x

data Bary a b c = Bary {abba :: c, daba :: a b} deriving (Show)

data Patient = Patient {
    firstName :: String
  , lastName  :: String
  , age       :: Int } deriving (Show)

patient = Patient {
      firstName = "John"
    , lastName  = "Smith"
    , age       = 23
    }

instance Funct' Maybe where
  fmap' _ Nothing = Nothing
  fmap' f (Just a) = Just (f a)

instance Funct' (Either a) where
  fmap' _ (Left a)  = Left a
  fmap' f (Right b) = Right (f b)

instance Funct' (Bary a b) where
  fmap' f (Bary {abba = x, daba = y}) = Bary {abba = f x, daba = y}
