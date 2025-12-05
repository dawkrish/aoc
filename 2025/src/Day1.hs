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
part1 file = sum $ map zerosEndOfRotation positions
  where
    positions = scanl (\acc (step, dir) -> move' (newPos acc) step dir) (Result 50 0 0) (map parse $ lines file)

part2 :: String -> Int
part2 file = x + y
  where
    x = sum $ map zerosEndOfRotation positions
    y = sum $ map zerosDuringRotation positions
    positions = scanl (\acc (step, dir) -> move' (newPos acc) step dir) (Result 50 0 0) (map parse $ lines file)

data Direction = L | R
  deriving (Eq, Show)

parse :: String -> (Int, Direction)
parse ('L' : x) = (read x :: Int, L)
parse ('R' : x) = (read x :: Int, R)

data Result = Result {newPos :: Int, zerosEndOfRotation :: Int, zerosDuringRotation :: Int}
  deriving (Show, Eq)

move' :: Int -> Int -> Direction -> Result
move' pos step dir =
  Result
    { newPos = finalPosition,
      zerosEndOfRotation = if finalPosition == 0 then 1 else 0,
      zerosDuringRotation = if finalPosition == 0 then rotationsInStep else rotationsInStep + crossedZero
    }
  where
    rotationsInStep = step `div` 100
    finalPosition = nextPos + (-(sign * rotationRequired * 100))
    crossedZero = if rotationRequired == 1 && pos /= 0 then 1 else 0
    rotationRequired = if nextPos < 0 || nextPos > 99 then 1 else 0
    nextPos = pos + (sign * netStep)
    sign = if dir == L then -1 else 1
    netStep = step `mod` 100

{-
>>> file <- readFile "samples/01.txt"
>>> part2' file
[Result {newPos = 50, zerosEndOfRotation = 0, zerosDuringRotation = 0},Result {newPos = 82, zerosEndOfRotation = 0, zerosDuringRotation = 1},Result {newPos = 52, zerosEndOfRotation = 0, zerosDuringRotation = 0},Result {newPos = 0, zerosEndOfRotation = 1, zerosDuringRotation = 0},Result {newPos = 95, zerosEndOfRotation = 0, zerosDuringRotation = 0},Result {newPos = 55, zerosEndOfRotation = 0, zerosDuringRotation = 1},Result {newPos = 0, zerosEndOfRotation = 1, zerosDuringRotation = 0},Result {newPos = 99, zerosEndOfRotation = 0, zerosDuringRotation = 0},Result {newPos = 0, zerosEndOfRotation = 1, zerosDuringRotation = 0},Result {newPos = 14, zerosEndOfRotation = 0, zerosDuringRotation = 0},Result {newPos = 32, zerosEndOfRotation = 0, zerosDuringRotation = 1}]
-}
part2' :: String -> [Result]
part2' file = positions
  where
    x = sum $ map zerosEndOfRotation positions
    y = sum $ map zerosDuringRotation positions
    positions = scanl (\acc (step, dir) -> move' (newPos acc) step dir) (Result 50 0 0) (map parse $ lines file)
