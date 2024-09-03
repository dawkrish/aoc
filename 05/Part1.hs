import Data.Char


main :: IO ()
main = do
  file <- readFile "input.txt"
  let sentences = lines file
  let seedLine = drop 7 . head $ sentences
  let seeds = map getNumber (words seedLine)
  let categPara = drop 2 sentences
  let categString = splitByEmptyLines [] categPara
  let categs = getCategs categString
  let finalSeeds = map (`location` categs) seeds
  -- print seeds
  -- print finalSeeds
  let answer = minimum finalSeeds
  print answer

data Categ = Categ {name :: String, entries :: [Entry]}
  deriving (Show)

data Entry = Entry {dest :: Int, src :: Int, rng :: Int}
  deriving (Show)

location :: Int -> [Categ] -> Int
location = foldl foo

foo :: Int -> Categ -> Int
foo x (Categ _ ents) = if all (== (-1)) findings then x else head (filter (/= (-1)) findings)
  where
    findings = [if x >= s && x <= s + r - 1  then x - s + d else -1 | (Entry d s r) <- ents]

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
