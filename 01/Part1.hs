import Data.Char

main :: IO ()
main = do
  file <- readFile "input.txt"
  let sentences = lines file
  let vals  = map val sentences
  print  $ sum vals

val :: String -> Int
val xs = head ds* 10 + last ds
  where ds = digits xs

digits :: String -> [Int]
digits xs = [digitToInt x | x <- xs, isDigit x]


