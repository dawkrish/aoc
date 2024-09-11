import Data.Char (ord)

main :: IO ()
main = do
  file <- readFile "input.txt"
  let line = init file
  let seqs = split ',' line ""
  let hashes = map hash seqs
  let answer = sum hashes
  print answer

hash :: String -> Int
hash = foldl (\acc x -> ((acc + ord x) * 17) `mod` 256) 0

split :: Char -> String -> String -> [String]
split c [] w = [w]
split c (x : xs) w
  | c == x = w : split c xs ""
  | otherwise = split c xs (w ++ [x])
