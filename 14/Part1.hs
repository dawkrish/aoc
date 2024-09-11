import Data.List

main :: IO ()
main = do
  file <- readFile "foo.txt"
  let sentences = lines file
  let c = cyc sentences
  -- mapM_ putStrLn c
  putStrLn ""
  let rep = iterate cyc sentences !! 1
  let loads = zip rep [length sentences, length sentences - 1 ..]
  let prods = zipWith (*) (map (length . zeros) rep) [length sentences, length sentences - 1 ..]
  let answer = sum prods
  let bar = map (\x -> sum $ zipWith (*) (map (length . zeros) (iterate cyc sentences !! x)) [length sentences, length sentences - 1 ..]) [0 .. 10000]
  let dd = detectCycle (drop 2 bar) []
  
  print dd

-- print (filter (\(x, y) -> x == 64) (zip bar [1 ..]))

detectCycle :: [Int] -> [Int] -> Int
detectCycle (x : xs) [] = detectCycle xs [x]
detectCycle (x : xs) cs
  | take (length cs) (x : xs) == cs && length cs > 2 = length cs
  | otherwise = detectCycle xs (cs ++ [x])

cyc :: [String] -> [String]
cyc = east . south . west . north

zeros :: String -> [Int]
zeros xs = map fst $ filter (\(_, x) -> x == 'O') (zip [0 ..] xs)

dots :: String -> [Int]
dots xs = map fst $ filter (\(_, x) -> x == '.') (zip [0 ..] xs)

north :: [String] -> [String]
north xs = iterate fall xs !! (length xs - 1)
  where
    fall ys = foldl (\acc x -> init acc ++ northP (last acc) x) [head ys] (tail ys)

northP :: String -> String -> [String]
northP xs ys = [xs', ys']
  where
    xs' = [if xs !! i == '.' && elem i tobe then 'O' else xs !! i | i <- [0 .. length xs - 1]]
    ys' = [if ys !! i == 'O' && elem i available then '.' else ys !! i | i <- [0 .. length ys - 1]]
    available = dots xs
    tobe = zeros ys

east :: [String] -> [String]
east = transpose . south . transpose

west :: [String] -> [String]
west = transpose . north . transpose

south :: [String] -> [String]
south xs = iterate fall xs !! (length xs - 1)
  where
    fall ys = foldr (\x acc -> southP (head acc) x ++ tail acc) [last ys] (init ys)

southP :: String -> String -> [String]
southP xs ys = [ys', xs']
  where
    xs' = [if xs !! i == '.' && elem i tobe then 'O' else xs !! i | i <- [0 .. length xs - 1]]
    ys' = [if ys !! i == 'O' && elem i available then '.' else ys !! i | i <- [0 .. length ys - 1]]
    available = dots xs
    tobe = zeros ys
