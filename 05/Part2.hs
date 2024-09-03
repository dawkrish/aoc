import Data.Char
import Data.List

main :: IO ()
main = do
  file <- readFile "2.txt"
  let sentences = lines file
  let seedLine = drop 7 . head $ sentences
  let seedArr = map getNumber (words seedLine)
  let seedPairs = adjacentPairs seedArr
  let allSeeds = concatMap expandRange seedPairs

  --  mapM_ print seedPairs

  let categPara = drop 2 sentences
  let categString = splitByEmptyLines [] categPara
  let categs = getCategs categString

  let locations = map (`findLocation` categs) allSeeds
  print (minimum locations)

data Categ = Categ {name :: String, entries :: [Entry]}
  deriving (Show)

data Entry = Entry {dest :: Int, src :: Int, rng :: Int}
  deriving (Show)

expandRange :: (Int, Int) -> [Int]
expandRange (a, b) = [a .. (a + b - 1)]

adjacentPairs :: [Int] -> [(Int, Int)]
adjacentPairs [] = []
adjacentPairs (x : y : xs) = (x, y) : adjacentPairs xs

findLocation :: Int -> [Categ] -> Int
findLocation = foldl' findNext

findNext :: Int -> Categ -> Int
findNext x (Categ _ ents) = foldl' isPresentInEntry x ents

isPresentInEntry :: Int -> Entry -> Int
isPresentInEntry x (Entry d s r)
  | x >= s && x <= s + r - 1 = x - s + d
  | otherwise = x

getCategs :: [[String]] -> [Categ]
getCategs cgs =
  [ Categ {name = desc, entries = ents}
    | cg <- cgs,
      let desc = head cg,
      let foo = tail cg,
      let bar = map words foo,
      let ents = [Entry (getNumber (head e)) (getNumber (e !! 1)) (getNumber (last e)) | e <- bar]
  ]

getNumber :: String -> Int
getNumber = foldl (\acc x -> acc * 10 + digitToInt x) 0

splitByEmptyLines :: [String] -> [String] -> [[String]]
splitByEmptyLines ls [] = [ls]
splitByEmptyLines ls (x : xs)
  | x == "" = ls : splitByEmptyLines [] xs
  | otherwise = splitByEmptyLines (ls ++ [x]) xs
