module Day2 where

import Data.List (sort)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  file <- readFile "inputs/Day2.txt"
  let sentences = lines file
  let ans1 = part1 sentences
  print ("Answer of part1 is " ++ show ans1)
  let ans2 = part2 sentences
  print ("Answer of part2 is " ++ show ans2)

part1 :: [String] -> Int
part1 = sum . map boxSurface1

part2 :: [String] -> Int
part2 = sum . map boxSurface2

boxSurface1 :: String -> Int
boxSurface1 xs =
  let [l, w, h] = (\x -> read x :: Int) <$> splitOn "x" xs
   in 2 * l * w + 2 * w * h + 2 * h * l + minimum [l * w, w * h, h * l]

boxSurface2 :: String -> Int
boxSurface2 dimStr =
  let [a, b, c] = sort $ read <$> splitOn "x" dimStr
   in 2 * (a + b) + a * b * c
