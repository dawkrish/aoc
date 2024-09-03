import Data.Char

main :: IO()
main = do
    file <- readFile "input.txt"
    let sentences = lines file
    let time =  getNumber . concat . words . drop 5 . head $ sentences
    let distance = getNumber . concat . words. drop 9 . last $  sentences
    
    print time
    print distance
    print (ways time distance)


getNumber :: String -> Int
getNumber = foldl (\acc x -> acc * 10 + digitToInt x) 0

ways :: Int -> Int -> Int
ways t d = length . filter (>d) $ possibilities
    where
     possibilities = map (\x -> x*t - x*x) [1..(t-1)]

