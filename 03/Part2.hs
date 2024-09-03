import Data.Char

main :: IO ()
main = do
  file <- readFile "input.txt"
  let sentences = lines file
  let nR = length sentences
  let nC = length (head sentences)
  let idxs = [0 .. nR - 1]
  let predicate (i, j) = and [i >= 0, j >= 0, i <= nR - 1, j <= nC - 1]

  let numbers = concat (zipWith (\sent idx -> parseNumber "" idx 0 sent) sentences idxs)
  let totalNumbers = length numbers
  let numberPairs =
        [ (numbers !! i, numbers !! j)
          | i <- [0 .. totalNumbers - 1],
            let x = numbers !! i,
            j <- [i + 1 .. totalNumbers - 1],
            let y = numbers !! j,
            abs (row x - row y) <= 2
        ]

  let symbols = concat (zipWith (\sent idx -> parseSymbols idx 0 sent) sentences idxs)

  let validPairs = concatMap (isValidConnectingStar numberPairs predicate) symbols

  let validRatios = map (\(x, y) -> number x * number y) validPairs
  let answer = sum validRatios
  print answer

data PartNumber = PartNumber
  { number :: Int,
    row :: Int,
    colStart :: Int,
    colEnd :: Int
  }
  deriving (Show, Eq)

isValidConnectingStar :: [(PartNumber, PartNumber)] -> ((Int, Int) -> Bool) -> (Int, Int) -> [(PartNumber, PartNumber)]
isValidConnectingStar pairs pred pos = filter (\(x, y) -> pos `elem` intersect (validPositions pred x) (validPositions pred y)) pairs

intersect :: (Eq a) => [a] -> [a] -> [a]
intersect xs ys = [x | x <- xs, y <- ys, x == y]

validPositions :: ((Int, Int) -> Bool) -> PartNumber -> [(Int, Int)]
validPositions p (PartNumber _ r a b) = filter p (top ++ mid ++ bot)
  where
    top = [(r - 1, j) | j <- horizontalRange]
    mid = [(r, a - 1), (r, b + 1)]
    bot = [(r + 1, j) | j <- horizontalRange]
    horizontalRange = [(a - 1) .. (b + 1)]

parseSymbols :: Int -> Int -> String -> [(Int, Int)]
parseSymbols _ _ [] = []
parseSymbols r c (x : xs)
  | x == '*' = (r, c) : parseSymbols r (c + 1) xs
  | otherwise = parseSymbols r (c + 1) xs

parseNumber :: String -> Int -> Int -> String -> [PartNumber]
parseNumber w r c []
  | not (null w) = [PartNumber (getNumber w) r (c - length w) (c - 1)]
  | otherwise = []
parseNumber w r c (x : xs)
  | isDigit x = parseNumber (w ++ [x]) r (c + 1) xs
  | otherwise = case w of
      [] -> parseNumber w r (c + 1) xs
      otherwise -> PartNumber (getNumber w) r (c - length w) (c - 1) : parseNumber "" r (c + 1) xs

getNumber :: String -> Int
getNumber = foldl (\acc x -> acc * 10 + digitToInt x) 0
