import Data.Char

main :: IO()
main = do
    file <- readFile "input.txt"
    let sentences = lines file
    let times = map getNumber . words . drop 5 . head $ sentences
    let distances = map getNumber . words. drop 9 . last $  sentences
    let pairs = zip times distances
    let wins  = map ways pairs
    let answer = product wins
    print answer


getNumber :: String -> Int
getNumber = foldl (\acc x -> acc * 10 + digitToInt x) 0

ways :: (Int, Int) -> Int
ways (t,d) = length . filter (>d) $ possibilities
    where
     possibilities = map (\x -> x*t - x*x) [1..(t-1)]
