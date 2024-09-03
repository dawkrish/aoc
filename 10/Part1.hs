import Data.Map qualified as Map

main :: IO ()
main = do
  file <- readFile "sample1.txt"
  let sentences = lines file
  let rows = length sentences
  let cols = length (head sentences)
  let coords = [(i, j) | i <- [0 .. rows - 1], j <- [0 .. cols - 1]]
  let chars = concat sentences
  let pairs = zip coords chars
  let np = toLoop pairs
  print np

toLoop :: [((Int, Int), Char)] -> [((Int, Int), Char)]
toLoop xs = xs
  where
    start = head [x | x@(p, c) <- xs, c == 'S']
