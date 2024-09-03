import Data.Char

main :: IO ()
main = do
  file <- readFile "input.txt"
  let sentences = lines file
  let n = length sentences
  let idxs = [0 .. n - 1]
  let vals = replicate n 1

  let foo = foldl (\acc i -> update i (acc !! i) (sentences !! i) acc) vals idxs
  let answer = sum foo
  print answer

update :: Int -> Int -> String -> [Int] -> [Int]
update idx copies sentence acc =
  [ if i >= (idx + 1) && i <= (idx + comp) then acc !! i + copies else acc !! i
    | let comp = compute sentence,
      i <- [0 .. length acc - 1]
  ]

compute :: String -> Int
compute xs = matching
  where
    matching = foldl (\acc x -> if x `elem` mynums then acc + 1 else acc) 0 winning
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
