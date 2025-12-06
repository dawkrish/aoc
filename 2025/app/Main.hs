module Main where

import qualified Day1
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let day = getDay args
  runDay day
  putStrLn "Hello, Haskell!"

getDay :: [String] -> Maybe Int
getDay [] = Nothing
getDay (x : _) = read x

runDay :: Maybe Int -> IO ()
runDay Nothing = putStrLn "unable to parse day"
runDay (Just x) = case x of
  1 -> Day1.run
  2 -> Day2.run
  3 -> Day3.run
  4 -> Day4.run
  5 -> Day5.run
  6 -> Day6.run
  7 -> Day7.run
  8 -> Day8.run
  9 -> Day9.run
  10 -> Day10.run
  11 -> Day11.run
  12 -> Day12.run
  _ -> putStrLn "Invalid day"
