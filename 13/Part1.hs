import Data.List

main :: IO ()
main = do
  file <- readFile "input.txt"
  let sentences = lines file
  let ps = paragraphs sentences []
  -- mapM_ (putStrLn . unlines) ps
  let rs = map ((* 100) . rows) ps
  let cs = map cols ps
  let answer = sum rs + sum cs
  print answer

dissimmilar :: String -> String -> Int
dissimmilar xs ys = length [1 | i <- [0 .. length xs - 1], xs !! i /= ys !! i]

smudgeMatch :: [Int] -> Bool
smudgeMatch = all (==0)

cols :: [String] -> Int
cols xs = if null selection then 0 else snd . head $ selection
  where
    selection = filter fst (zip bools [1 ..])
    bools = map (\zs -> smudgeMatch [dissimmilar (ys !! (p - 1)) (ys !! (q - 1)) | (p, q) <- zs]) checks
    checks = [zip [i + 1 .. small + i] [i, i - 1 ..] | i <- [1 .. ncols - 1], let small = min (i - s) (e - i)]
    ys = transpose xs
    ncols = length . head $ xs
    s = 0
    e = ncols

rows :: [String] -> Int
rows xs = if null selection then 0 else snd . head $ selection
  where
    selection = filter fst (zip bools [1 ..])
    bools = map (\zs -> smudgeMatch [dissimmilar (xs !! (p - 1)) (xs !! (q - 1)) | (p, q) <- zs]) checks
    checks = [zip [i + 1 .. small + i] [i, i - 1 ..] | i <- [1 .. nrows - 1], let small = min (i - s) (e - i)]
    nrows = length xs
    s = 0
    e = nrows

paragraphs :: [String] -> [String] -> [[String]]
paragraphs [] p = [p]
paragraphs (x : xs) p
  | x == "" = p : paragraphs xs []
  | otherwise = paragraphs xs (p ++ [x])
