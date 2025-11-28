module Day1 where

main :: IO ()
main = do
  file <- readFile "inputs/Day1.txt"
  print ("file length is" ++ show (length file))
  let ans1 = part1 file
  print ("Answer of part1 is " ++ show ans1)
  let ans2 = part2 file
  print ("Answer of part2 is " ++ show ans2)


count :: Eq a => a -> [a] -> Int
count c = length . filter (== c)

part1 :: String -> Int
part1 = foldl (\x y -> if y == '(' then x + 1 else x - 1) 0 
 
part2 :: String  -> Int
part2 = length . takeWhile (>= 0) . scanl (\x y -> if y == '(' then x + 1 else x - 1) 0

