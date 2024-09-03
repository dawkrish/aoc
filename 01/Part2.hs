import Data.Char

main :: IO ()
main = do
  file <- readFile "input.txt"
  let sentences = lines file
  let vals = map val sentences
  print $ sum vals

pairs :: [(String, Int)]
pairs =
  [ ("zero", 0),
    ("one", 1),
    ("two", 2),
    ("three", 3),
    ("four", 4),
    ("five", 5),
    ("six", 6),
    ("seven", 7),
    ("eight", 8),
    ("nine", 9)
  ]

val :: String -> Int
val xs = head ds * 10 + last ds
  where
    ds = digits xs ""

digits :: String -> String -> [Int]
digits [] w = keys
  where
    keys = isKey pairs w
digits (x : xs) w
  | isDigit x = digitToInt x : digits xs w
  | null keys = digits xs (w ++ [x])
  | otherwise = head keys : digits xs [x]
  where
    keys = isKey pairs (w ++ [x])

isKey :: [(String, Int)] -> String -> [Int]
isKey pairs word = [v | (k, v) <- pairs, isSub word k k]

isSub :: String -> String -> String -> Bool
isSub _ [] _ = True
isSub [] _ _ = False
isSub (w : ws) (k : ks) original
  | w == k = isSub ws ks original
  | (k : ks) == original = isSub ws original original
  | otherwise = isSub (w : ws) original original
