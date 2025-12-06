module Day5 where

run :: IO ()
run = do
  file <- readFile "inputs/01.txt"
  let p1 = part1 file
  let p2 = part2 file
  putStrLn $ "Answer for part1:" ++ show p1
  putStrLn $ "Answer for part2:" ++ show p2
  return ()

part1 :: String -> Int
part1 file = 0

part2 :: String -> Int
part2 file = 0