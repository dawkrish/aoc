import Data.Char

main :: IO ()
main = do
  file <- readFile "input.txt"
  let sentences = lines file
  let ids = map getId sentences
  let zs = zip (map getId sentences) (map getColorsOfSentence sentences)
  let validZs = filter (\(s, xs) -> all isValidColorCombo xs) zs
  let validIds = [id | (id, colors) <- validZs]
  print (sum validIds)

isValidColorCombo :: [Int] -> Bool
isValidColorCombo (x : y : z : xs) = x <= 12 && y <= 13 && z <= 14

getColorsOfSentence :: String -> [[Int]]
getColorsOfSentence xs = map sectionToColor sections
  where
    sections = splitString idsAndSections ';' ""
    idsAndSections = concat . tail $ splitString xs ':' ""

sectionToColor :: String -> [Int]
sectionToColor section = bar
  where
    bar = transform foo
    foo = map (colorString2ColorCode "") cs
    cs = splitString t ',' ""
    t = trim section

transform :: [String] -> [Int]
transform xs = sumColumnsOf2dMatrix [foo x | x <- xs]
  where
    foo x
      | last x == 'r' = [getNumber (init x), 0, 0]
      | last x == 'g' = [0, getNumber (init x), 0]
      | last x == 'b' = [0, 0, getNumber (init x)]
      | otherwise = [0, 0, 0]

sumColumnsOf2dMatrix :: [[Int]] -> [Int]
sumColumnsOf2dMatrix = foldl (\acc x -> [(acc !! i) + (x !! i) | i <- [0 .. length acc - 1]]) [0, 0, 0]

colorString2ColorCode :: String -> String -> String
colorString2ColorCode w (x : xs)
  | not (isDigit x) = w ++ [x]
  | otherwise = colorString2ColorCode (w ++ [x]) xs

getId :: String -> Int
getId xs = getNumber . init $ (words xs !! 1)

getNumber :: String -> Int
getNumber = foldl (\acc x -> acc * 10 + digitToInt x) 0

trim :: String -> String
trim xs = [x | x <- xs, x /= ' ']

splitString :: String -> Char -> String -> [String]
splitString [] c w = [w]
splitString (x : xs) c w
  | x == c = w : splitString xs c ""
  | otherwise = splitString xs c (w ++ [x])
