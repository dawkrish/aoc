module Main where

import qualified Day1 as Day1

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"

runDay :: Int -> IO ()
runDay 1 = Day1.run
