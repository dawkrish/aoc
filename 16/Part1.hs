main = do
  file <- readFile "foo.txt"
  let sentences = lines file
  let lights = [[('R', (0, 0))]]

  let newLights = run lights sentences
  mapM_ print newLights

run :: [[(Char, (Int, Int))]] -> [String] -> [(Char, (Int, Int))]
run lights grid = run (map foo lights) grid
  where
    coord = snd . head
    chr = fst . head
    foo l = case chr l of
      'R' -> case pos (coord l) grid of
        '.' -> ('R', right (coord l)) : l


pos :: (Int, Int) -> [[a]] -> a
pos (a, b) grid = (grid !! a) !! b

right :: (Int, Int) -> (Int, Int)
right (a, b) = (a, b + 1)

left :: (Int, Int) -> (Int, Int)
left (a, b) = (a, b - 1)

up :: (Int, Int) -> (Int, Int)
up (a, b) = (a - 1, b)

down :: (Int, Int) -> (Int, Int)
down (a, b) = (a + 1, b)
