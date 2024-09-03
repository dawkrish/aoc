import Data.List

main = do
  file <- readFile "input.txt"
  let sentences = lines file
  let badRows = map snd $ filter (\(x, _) -> all (== '.') x) (zip sentences [0 ..])
  let badCols = map snd $ filter (\(x, _) -> all (== '.') x) (zip (transpose sentences) [0 ..])
  let coords = [(i, j) | i <- [0 .. length sentences - 1], j <- [0 .. (length . head $ sentences) - 1]]
  let chars = concat sentences

  let galaxies = filter (\(c, _) -> c == '#') (zip chars coords)
  let galaxyPairs = uniquePairs galaxies
  let repAmount = 1000000
  let distances = map (\(x, y) -> dist x y badRows badCols repAmount) galaxyPairs

  print (sum distances)

expand :: [String] -> [String]
expand = foldl (\acc x -> if all (== '.') x then acc ++ concat (replicate 1 [x]) else acc ++ [x]) []

dist :: (Char, (Int, Int)) -> (Char, (Int, Int)) -> [Int] -> [Int] -> Int -> Int
dist (_, (x1, y1)) (_, (x2, y2)) rows cols rep = p + q
  where
    rowsBetween = length $ filter (\x -> x1 < x && x2 > x) rows
    colsBetween = length $ filter (\y -> (y1 > y && y2 < y) || (y1 < y && y2 > y)) cols
    p = abs (x1 - x2) + (rowsBetween * (rep - 1))
    q = abs (y1 - y2) + (colsBetween * (rep - 1))

conv :: Int -> Int -> Int -> Int
conv base to x
  | x <= base = x
  | otherwise = (x `div` base * to) + x `mod` base

uniquePairs :: [b] -> [(b, b)]
uniquePairs xs = [(xs !! i, xs !! j) | i <- [0 .. length xs - 1], j <- [i .. length xs - 1], i /= j]

