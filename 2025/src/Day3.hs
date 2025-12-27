module Day3 where

run :: IO ()
run = do
  file <- readFile "inputs/03.txt"
  let p1 = part1 file
  let p2 = part2 file
  putStrLn $ "Answer for part1: " ++ show p1
  putStrLn $ "Answer for part2: " ++ show p2
  return ()

part1 :: String -> Int
part1 file = sum $ map (process . parse) (lines file)

part2 :: String -> Int
part2 file = 0

parse :: String -> [Int]
parse = map (\x -> read [x] :: Int)

process :: [Int] -> Int
process xs = maximum $ go xs []
  where
    go [] acc = acc
    go [_] acc = acc
    go (y : ys) acc = go ys ((10 * y) + maximum ys : acc)
