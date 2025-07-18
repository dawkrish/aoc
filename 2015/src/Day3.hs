{-# LANGUAGE OverloadedStrings #-}

module Day3 where

import Data.Set (Set, empty, insert, size)

main :: IO ()
main = do
  file <- readFile "inputs/Day3.txt"
  print ("file length is " ++ show (length file))
  let ans1 = part1 file (MkCord 0 0) empty
  print ("Answer of part1 is " ++ show ans1)
  let ans2 = part2 file Santa (MkCord 0 0, MkCord 0 0) empty
  print ("Answer of part22 is " ++ show ans2)
  return ()

data Coord = MkCord {x :: Int, y :: Int}
  deriving (Show, Eq, Ord)

data Chance = Santa | Robocop

part1 :: String -> Coord -> Set Coord -> Int
part1 [] pos s = size (insert pos s)
part1 (x : xs) pos@(MkCord a b) s = case x of
  '>' -> part1 xs (pos {x = a + 1}) (insert pos s)
  '<' -> part1 xs (pos {x = a - 1}) (insert pos s)
  '^' -> part1 xs (pos {y = b - 1}) (insert pos s)
  'v' -> part1 xs (pos {y = b + 1}) (insert pos s)
  _ -> part1 xs pos (insert pos s)

alternate :: [a] -> ([a], [a])
alternate = foldr (\x (as, bs) -> (bs, x : as)) ([], [])

part2 :: String -> Chance -> (Coord, Coord) -> Set Coord -> Int
part2 [] Santa (p, _) s = size (insert p s)
part2 (x : xs) Santa (p@(MkCord a b), q) s = case x of
  '>' -> part2 xs Robocop (p {x = a + 1}, q) (insert p s)
  '<' -> part2 xs Robocop (p {x = a - 1}, q) (insert p s)
  '^' -> part2 xs Robocop (p {y = b - 1}, q) (insert p s)
  'v' -> part2 xs Robocop (p {y = b + 1}, q) (insert p s)
  _ -> part2 xs Robocop (p, q) (insert p s)
part2 [] Robocop (_, q) s = size (insert q s)
part2 (x : xs) Robocop (p, q@(MkCord a b)) s = case x of
  '>' -> part2 xs Santa (p, q {x = a + 1}) (insert q s)
  '<' -> part2 xs Santa (p, q {x = a - 1}) (insert q s)
  '^' -> part2 xs Santa (p, q {y = b - 1}) (insert q s)
  'v' -> part2 xs Santa (p, q {y = b + 1}) (insert q s)
  _ -> part2 xs Santa (p, q) (insert q s)

{-
Code generated by AI based on my code, a more idiomatic version

module Day3 where

import Data.Set (Set, empty, insert, size)
import qualified Data.Set as Set

main :: IO ()
main = do
  file <- readFile "inputs/Day3.txt"
  print ("file length is " ++ show (length file))
  let start = MkCoord 0 0
      ans1 = part1 file start Set.empty
      ans2 = part2 file Santa (start, start) Set.empty
  print ("Answer of part1 is " ++ show ans1)
  print ("Answer of part2 is " ++ show ans2)

-- === Data Definitions ===

data Coord = MkCoord { x :: Int, y :: Int }
  deriving (Show, Eq, Ord)

data Chance = Santa | Robocop
  deriving (Show, Eq)

-- === Movement Logic ===

move :: Char -> Coord -> Coord
move '>' c = c { x = x c + 1 }
move '<' c = c { x = x c - 1 }
move '^' c = c { y = y c - 1 }
move 'v' c = c { y = y c + 1 }
move _   c = c  -- Ignore unknown characters

-- === Part 1 ===

part1 :: String -> Coord -> Set Coord -> Int
part1 [] pos visited = size (insert pos visited)
part1 (c:cs) pos visited =
  let newPos = move c pos
  in part1 cs newPos (insert pos visited)

-- === Part 2 ===

part2 :: String -> Chance -> (Coord, Coord) -> Set Coord -> Int
part2 [] _ (p, q) visited = size (insert p (insert q visited))
part2 (c:cs) Santa (p, q) visited =
  let newP = move c p
  in part2 cs Robocop (newP, q) (insert p visited)
part2 (c:cs) Robocop (p, q) visited =
  let newQ = move c q
  in part2 cs Santa (p, newQ) (insert q visited)

-}