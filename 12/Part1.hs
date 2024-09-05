import Data.Char

main = do
  file <- readFile "foo.txt"
  let sentences = lines file
  mapM_ (print . compute') sentences

compute' xs = (dots, hashs, ques, info)
  where
    ws = words xs
    springs = head ws
    dots = getSpec springs '.'
    hashs = getSpec springs '#'
    ques = getSpec springs '?'
    info = getNumbers (last ws) ""

compute :: String -> Int
compute xs = helper dots hashs ques info
  where
    ws = words xs
    springs = head ws
    dots = getSpec springs '.'
    hashs = getSpec springs '#'
    ques = getSpec springs '?'
    info = getNumbers (last ws) ""

helper :: [Int] -> [Int] -> [Int] -> [Int] -> Int
helper dots hashs ques info = 0
  where chars = length dots +  length hashs + length ques

getSpec :: String -> Char -> [Int]
getSpec xs c = filter (\i -> xs !! i == c) [0 .. length xs - 1]

getNumbers :: String -> String -> [Int]
getNumbers [] w = [toNum w]
getNumbers (x : xs) w
  | x == ',' = toNum w : getNumbers xs ""
  | otherwise = getNumbers xs (w ++ [x])

toNum :: String -> Int
toNum = foldl (\acc x -> acc * 10 + digitToInt x) 0
