import Data.Char

main :: IO ()
main = do
  file <- readFile "input.txt"
  let sentences = lines file
  let numberString = map words sentences
  let numbers = map (map getNumber) numberString
  let histories = map (history 0) numbers
  --mapM_ print (zip numbers histories)
  print (sum histories)

history :: Int -> [Int] -> Int
history n xs
  | allZeros xs = n
  | otherwise = history (n + last xs) (diff xs)

diff :: [Int] -> [Int]
diff xs = zipWith (-) (tail xs) xs

allZeros :: [Int] -> Bool
allZeros = all (== 0)

getNumber :: String -> Int
getNumber xs =
  if head xs /= '-'
    then foldl (\acc x -> acc * 10 + digitToInt x) 0 xs
    else -(1 * foldl (\acc x -> acc * 10 + digitToInt x) 0 (tail xs))
