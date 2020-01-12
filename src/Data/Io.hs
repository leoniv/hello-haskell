module Data.Io where

import Control.Monad
import Control.Exception
import System.IO

getColors = do
  colors <- forM [1,2,3] (\a -> do
    color <- getLine
    return color)
  return colors

openFileR = do
  flip openFile ReadMode

readFile' path f = do
  bracket (openFile path ReadMode) hClose (\handle -> f handle)

putsContent = (\handle -> do
  content <- hGetContents handle
  putStr content)
