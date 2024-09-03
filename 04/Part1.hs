import Data.Char

main :: IO ()
main = do
  file <- readFile "input.txt"
  let sentences = lines file
  let computes = map compute sentences
  let answer = sum computes
  print answer

compute :: String -> Int
compute xs = if matching == 0 then 0 else 2 ^ (matching - 1)
  where
    matching = foldl (\acc x -> if elem x mynums then acc + 1 else acc) 0 winning
    winning = parseNumbers "" first
    mynums = parseNumbers "" second
    first = getFirstHalf rmv
    second = getSecondHalf rmv
    rmv = removeCard xs

parseNumbers :: String -> String -> [Int]
parseNumbers w []
  | null w = []
  | otherwise = [getNumber w]
parseNumbers w (x : xs)
  | isDigit x = parseNumbers (w ++ [x]) xs
  | null w = parseNumbers w xs
  | otherwise = getNumber w : parseNumbers "" xs

getFirstHalf :: String -> String
getFirstHalf = takeWhile (/= '|')

getSecondHalf :: String -> String
getSecondHalf = tail . dropWhile (/= '|')

removeCard :: String -> String
removeCard = tail . dropWhile (/= ':')

getNumber :: String -> Int
getNumber = foldl (\acc x -> acc * 10 + digitToInt x) 0
