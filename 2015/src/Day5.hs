module Day5 where

main :: IO ()
main = do
  fileText <- readFile "inputs/Day5.txt"
  let sentences = lines fileText
  let ans1 = sum (map eval sentences)
  putStrLn ("ans " ++ show ans1)
  return ()

eval :: String -> Int
eval xs =
  if vowels3 xs && rowTwice xs && not (blacklist xs)
    then 1
    else 0

vowels3 :: [Char] -> Bool
vowels3 xs = length (filter isAVowel xs) >= 3

isAVowel :: Char -> Bool
isAVowel c = c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u'

rowTwice :: [Char] -> Bool
rowTwice xs =
  any (\(p, q) -> p == q) (pairs xs)

blacklist :: [Char] -> Bool
blacklist xs =
  any
    ( \x ->
        x == ('a', 'b')
          || x == ('c', 'd')
          || x == ('p', 'q')
          || x == ('x', 'y')
    )
    (pairs xs)

pairs :: [b] -> [(b, b)]
pairs xs = zip xs (tail xs)
