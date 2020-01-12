module Data.RpnCalc where

import Control.Monad

solveRpn :: String -> Maybe Double
solveRpn st = do
  [result] <- foldM calcFunc [] . words $ st
  return result

calcFunc :: [Double] -> String -> Maybe [Double]
calcFunc stack "+"  = return'' stack (+)
calcFunc stack "*"  = return'' stack (*)
calcFunc stack "-"  = return'' stack (-)
calcFunc stack "^"  = return'' stack (**)
calcFunc stack "/"  = return'' stack (/)
calcFunc stack "ln" = return' stack log
calcFunc stack item = (:stack) <$> maybeRead item

return'' (x:y:stack) f = return ((x `f` y) : stack)
return' (x:stack) f = return (f x : stack)

maybeRead :: (Read a) => String -> Maybe a
maybeRead st = case reads st of
    [(x, "")] -> Just x
    _ -> Nothing
