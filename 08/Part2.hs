import Data.Map qualified as Map

main :: IO ()
main = do
  file <- readFile "input.txt"
  let sentences = lines file
  let instructions = head sentences
  let nodes = map getNode (drop 2 sentences)
  let nodeMap = Map.fromList nodes
  let startings = Map.toList $ Map.filterWithKey (\k _ -> last k == 'A') nodeMap
  let ends = map (\s -> reachEnd s 0 (cycle instructions) nodeMap) startings
  let finalEnd = foldr lcm 1 ends
  print finalEnd

getNode :: String -> (String, (String, String))
getNode xs = (a, (b, c))
  where
    a = take 3 xs
    ys = init . drop 7 $ xs
    b = take 3 ys
    c = drop 5 ys

reachEnd :: (String, (String, String)) -> Int -> String -> Map.Map String (String, String) -> Int
reachEnd start count (x : xs) nmap
  | last (fst start) == 'Z' = count
  | x == 'L' = reachEnd (fst . snd $ start, maplookup (fst . snd $ start) nmap) (count + 1) xs nmap
  | x == 'R' = reachEnd (snd . snd $ start, maplookup (snd . snd $ start) nmap) (count + 1) xs nmap

maplookup :: (Ord k) => k -> Map.Map k a -> a
maplookup key m = case Map.lookup key m of (Just x) -> x
