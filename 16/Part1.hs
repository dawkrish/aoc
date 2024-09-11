main = do
  file <- readFile "foo.txt"
  let sentences = lines file
  let lights = [[(0, 0, 'R')]]

  let newLights = run lights sentences
  print newLights

run :: [[(Int, Int, Char)]] -> [String] -> [[(Int, Int, Char)]]
run lights grid = run (concatMap foo lights) grid
  where
    foo l = case (grid !! (t1 . head $ l)) !! (t2 . head $ l) of
      '.' -> [(t1 . head $ l, 1 + (t2 . head $ l), t3 . head $ l) : l]

t1 :: (a, b, c) -> a
t1 (x, _, _) = x

t2 :: (a, b, c) -> b
t2 (_, y, _) = y

t3 :: (a, b, c) -> c
t3 (_, _, z) = z
