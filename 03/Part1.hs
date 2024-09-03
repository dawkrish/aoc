import Data.Char

main :: IO ()
main = do
  file <- readFile "input.txt"
  let sentences = lines file
  let nR = length sentences
  let nC = length (head sentences)
  let idxs = [0 .. nR - 1]
  let numbers = concat (zipWith (\sent idx -> parseNumber "" idx 0 sent) sentences idxs)
  let symbols = concat (zipWith (\sent idx -> parseSymbols idx 0 sent) sentences idxs)
  let predicate (i, j) = and [i >= 0, j >= 0, i <= nR - 1, j <= nC - 1]
  let go ss vps = any (`elem` vps) ss
  let validNumbers = filter (\(PartNumber _ r a b) -> go symbols (validPositions predicate r a b)) numbers
  let numericValidNumbers = map number validNumbers
  let answer = sum numericValidNumbers
  -- mapM_ print numbers
  print answer

data PartNumber = PartNumber
  { number :: Int,
    row :: Int,
    colStart :: Int,
    colEnd :: Int
  }
  deriving (Show)

validPositions :: ((Int, Int) -> Bool) -> Int -> Int -> Int -> [(Int, Int)]
validPositions p r a b = filter p (top ++ mid ++ bot)
  where
    top = [(r - 1, j) | j <- horizontalRange]
    mid = [(r, a-1), (r, b+1)]
    bot = [(r + 1, j) | j <- horizontalRange]
    horizontalRange = [(a - 1) .. (b + 1)]

parseSymbols :: Int -> Int -> String -> [(Int, Int)]
parseSymbols _ _ [] = []
parseSymbols r c (x : xs)
  | x == '.' || isDigit x = parseSymbols r (c + 1) xs
  | otherwise = (r, c) : parseSymbols r (c + 1) xs

parseNumber :: String -> Int -> Int -> String -> [PartNumber]
parseNumber w r c [] = [PartNumber (getNumber w) r (c - length w) (c - 1)]
parseNumber w r c (x : xs)
  | isDigit x = parseNumber (w ++ [x]) r (c + 1) xs
  | otherwise = case w of
      [] -> parseNumber w r (c + 1) xs
      otherwise -> PartNumber (getNumber w) r (c - length w) (c - 1) : parseNumber "" r (c + 1) xs

getNumber :: String -> Int
getNumber = foldl (\acc x -> acc * 10 + digitToInt x) 0
