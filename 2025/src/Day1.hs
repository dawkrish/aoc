module Day1 where

run :: IO ()
run = do
  file <- readFile "inputs/01.txt"
  let p1 = part1 file
  let p2 = part2 file
  putStrLn $ "Answer for part1:" ++ show p1
  putStrLn $ "Answer for part2:" ++ show p2
  return ()

part1 :: String -> Int
part1 file = length $ filter (== 0) positions
  where
    positions = scanl (\acc (step, dir) -> move acc step dir) 50 (map parse $ lines file)

part2 :: String -> Int
part2 file = 0

data Direction = L | R
  deriving (Eq, Show)

parse :: String -> (Int, Direction)
parse ('L' : x) = (read x :: Int, L)
parse ('R' : x) = (read x :: Int, R)

move :: Int -> Int -> Direction -> Int
move pos step L
  | pos - resolvedStep >= 0 = pos - resolvedStep
  | otherwise = pos - resolvedStep + 100
  where
    resolvedStep = step `mod` 100
move pos step R
  | pos + resolvedStep <= 99 = pos + resolvedStep
  | otherwise = pos + resolvedStep - 100
  where
    resolvedStep = step `mod` 100

data Result = Result {newPos :: Int, zerosEndOfRotation :: Int, zerosDuringRotation :: Int}

move' :: Int -> Int -> Direction -> Int
move' pos step dir = pos + (sign * netStep) + (-sign * 100 * crossZero)
  where
    netStep = step `mod` 100
    rotationsInStep = step `div` 100
    crossZero = 0
    sign = if dir == L then -1 else 1
