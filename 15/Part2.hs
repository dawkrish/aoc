import Data.Char (digitToInt, isAlpha, ord)

main :: IO ()
main = do
  file <- readFile "input.txt"
  let line = init file
  let seqs = split ',' line ""
  let labels = map (takeWhile isAlpha) seqs
  let ops = map (dropWhile isAlpha) seqs
  let boxes = replicate 256 []
  let hashes = map hash labels
  let foo = run boxes labels ops hashes
  let zpd = zip foo [1 ..]
  let mpd = concatMap (\(box, idx) -> [j * idx * snd (box !! (j - 1)) | j <- [1 .. length box]]) zpd
  print (sum mpd)

run :: [[(String, Int)]] -> [String] -> [String] -> [Int] -> [[(String, Int)]]
run boxes _ _ [] = boxes
run boxes (l : ls) (o : os) (x : xs)
  | o == "-" =
      run
        [ if i == x
            then filter (\(x, _) -> x /= l) box
            else box
          | i <- [0 .. 255],
            let box = boxes !! i
        ]
        ls
        os
        xs
  | otherwise =
      run
        [ if i == x
            then
              ( if l `elem` map fst box
                  then [if fst lens == l then (l, fl) else lens | lens <- box]
                  else box ++ [(l, fl)]
              )
            else box
          | i <- [0 .. 255],
            let box = boxes !! i,
            let fl = digitToInt (last o)
        ]
        ls
        os
        xs

hash :: String -> Int
hash = foldl (\acc x -> ((acc + ord x) * 17) `mod` 256) 0

split :: Char -> String -> String -> [String]
split c [] w = [w]
split c (x : xs) w
  | c == x = w : split c xs ""
  | otherwise = split c xs (w ++ [x])
