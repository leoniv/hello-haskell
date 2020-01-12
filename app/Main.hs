module Main where

import System.Environment
import Data.List
import System.Exit
import System.IO

main :: IO ()
main = do
  args <- getArgs
  if null args then undefComand else
    let (comand:args') = args in dispatch comand args'

usage progname =
  ("Usage: `" ++ progname ++ " command args'") :
    map ((" " ++ progname ++ " ") ++)
      [ "add path \"string\""
       ,"view path num"
       ,"remove path num"
      ]

putsError =
  hPutStrLn stderr

usage' = do
  progName <- getProgName
  mapM putsError (usage progName)
  return ()

undefComand = do
  usage'
  exitWith $ ExitFailure 1

unkownComand comand _ = do
  putsError ("ERR: Inavlid comand `" ++ comand ++ "`")
  usage'
  exitWith $ ExitFailure 1

dispatch :: String -> [String] -> IO ()
dispatch comand =
  case comand of
    "add" -> add
    "view" -> view
    "remove" -> remove
    othervise -> unkownComand comand

add :: [String] -> IO ()
add args = putStrLn . show $ args

view :: [String] -> IO ()
view args = return ()

remove :: [String] -> IO ()
remove args = return ()
