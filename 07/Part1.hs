import Data.Char
import Data.List

main :: IO ()
main = do
  file <- readFile "input.txt"
  let sentences = lines file
  let bids = map (last . words) sentences
  let hands = map (\x -> Hand (head . words $ x) (getHandRank (head . words $ x)) (getNumber . last . words $ x)) sentences
  let sortedHands = sort hands
  let bids = zipWith (*)  [1..]  (map bid sortedHands)
  let answer = sum bids
  print answer


data Hand = Hand {val :: String, rank :: Int, bid :: Int}
  deriving (Show)

instance Eq Hand where
  (==) h1 h2 = False

instance Ord Hand where
  compare (Hand val1 r1 _) (Hand val2 r2 _)
    | r1 > r2 = GT
    | r2 > r1 = LT
    | otherwise = compareHandString val1 val2

getNumber :: String -> Int
getNumber = foldl (\acc x -> acc * 10 + digitToInt x) 0

compareHandString :: String -> String -> Ordering
compareHandString (x:xs) (y:ys)
    | x == y = compareHandString xs ys
    | isAlpha x && isDigit y = GT
    | isDigit x && isAlpha y = LT
    | isDigit x && isDigit y = compare (digitToInt x) (digitToInt y)
    | x == 'A' =  GT
    | y == 'A' = LT
    | x == 'K' = GT
    | y == 'K' = LT
    | x == 'Q' = GT
    | y == 'Q' = LT
    | x == 'J' = GT
    | y == 'J' = LT


getHandRank :: String -> Int
getHandRank xs
  | is5Kind xs = 7
  | is4Kind xs = 6
  | isFullHouse xs = 5
  | is3Kind xs = 4
  | is2Pair xs = 3
  | is1Pair xs = 2
  | isHighCard xs = 1
  where
    freq = frequency xs

is5Kind :: String -> Bool
is5Kind xs = (length . frequency $ xs) == 1

is4Kind :: String -> Bool
is4Kind xs = length freq == 2 && (snd (head freq) == 1 || snd (last freq) == 1)
  where
    freq = frequency xs

isFullHouse :: String -> Bool
isFullHouse xs = length freq == 2 && (snd (head freq) == 2 || snd (last freq) == 2)
  where
    freq = frequency xs

is3Kind :: String -> Bool
is3Kind xs = length freq == 3 && any (\(_, x) -> x == 3) freq
  where
    freq = frequency xs

is2Pair :: String -> Bool
is2Pair xs = length freq == 3 && any (\(_, x) -> x == 2) freq
  where
    freq = frequency xs

is1Pair :: String -> Bool
is1Pair xs = length freq == 4
  where
    freq = frequency xs

isHighCard :: String -> Bool
isHighCard xs = (length . frequency $ xs) == 5

frequency :: String -> [(Char, Int)]
frequency = map (\x -> (head x, length x)) . group . sort
