module Day5 where

main :: IO ()
main = do
  fileText <- readFile "inputs/Day5.txt"
  let sentences = lines fileText
  let ans1 = sum (map eval sentences)
  let ans2 = sum (map eval2 sentences)
  putStrLn ("ans " ++ show ans1)
  putStrLn ("ans 2" ++ show ans2)
  return ()

eval :: String -> Int
eval xs =
  if vowels3 xs && rowTwice xs && not (blacklist xs)
    then 1
    else 0

eval2 xs =
  if pairsWithoutOverlap xs && (atleast1Repeat xs && (length xs == length (variant xs)))
    then 1
    else 0

atleast1Repeat xs = any (\(a, b, c) -> a == c) (triples xs)

variant [] = []
variant [x] = [x]
variant [x, y] = [x, y]
variant [x, y, z] = [x, y, z]
variant (p : q : r : s) = foldl (\acc x -> if last acc == x then acc else acc ++ [x]) [p, q, r] s

-- pairsWithoutOverlap :: String -> Bool
pairsWithoutOverlap xs = any (\x -> length (filter (== x) p) > 1) p
  where
    p = pairs (refined xs)

vowels3 :: String -> Bool
vowels3 xs = length (filter isAVowel xs) >= 3

isAVowel :: Char -> Bool
isAVowel c = c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u'

rowTwice :: String -> Bool
rowTwice xs =
  any (uncurry (==)) (pairs xs)

blacklist :: String -> Bool
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

triples xs = zipWith (\(a, b) c -> (a, b, c)) (zip xs (tail xs)) (tail (tail xs))

refined [] = []
refined [x] = [x]
refined [x, y] = [x, y]
refined (x : y : z) = foldl (\acc (a, b, c) -> if a == b && b == c then acc else acc ++ [c]) [x, y] (triples (x : y : z))
