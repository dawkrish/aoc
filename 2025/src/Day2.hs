module Day2 where

import Data.List.Split (splitOn)

run :: IO ()
run = do
  file <- readFile "samples/02.txt"
  let p1 = part1 file
  let p2 = part2 file
  putStrLn $ "Answer for part1: " ++ show p1
  putStrLn $ "Answer for part2: " ++ show p2
  print (part2' file)
  -- putStrLn $ "Answer for part2: " ++ show (part2' file)
  return ()

-- something is isInvalid if splitAt half gives equals
part1 :: String -> Int
part1 file = foldr (\x acc -> acc + processRange is2wayMirror x) 0 (parse file)

-- something is isInvalid if there is a repetition atleast twice
part2 :: String -> Int
part2 file = foldr (\x acc -> acc + processRange isNwayMirror x) 0 (parse file)

part2' file = map (\(x, y) -> map isNwayMirror [x .. y]) ranges
  where
    ranges = parse file

type Range = (Int, Int)

processRange :: (Int -> Bool) -> Range -> Int
processRange g (f, s) = (sum . filter g) [f .. s]

is2wayMirror :: Int -> Bool
is2wayMirror x = uncurry (==) (half . show $ x)
  where
    half y = splitAt (length y `div` 2) y

isNwayMirror :: Int -> Bool
isNwayMirror x = goFun (show x) "" ""

goFun [] ys rep = null ys
goFun (x : xs) [] rep = goFun xs [x] [x]
goFun (x : xs) (y : ys) rep
  | x /= y = goFun xs ((y : ys) ++ [x]) (rep ++ [x])
  | otherwise = goFun xs ys (rep ++ [x])

parse :: String -> [Range]
parse xs = map parseItems (splitOn "," xs)
  where
    parseItems pairs = ((read . head . splitByHyphen) pairs, (read . last . splitByHyphen) pairs)
    splitByHyphen = splitOn "-"
