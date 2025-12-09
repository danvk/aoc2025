-- https://adventofcode.com/2025/day/9

import Data.List
import Data.List.Split
import System.Environment (getArgs)

type Point = (Int, Int)

parseLine :: String -> Point
parseLine str = case splitOn "," str of
  [a, b] -> (read a, read b)
  _ -> error $ "Unable to parse " ++ str

area :: Point -> Point -> Int
area (x1, y1) (x2, y2) = (x2 - x1 + 1) * (y2 - y1 + 1)

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let pts = map parseLine $ lines content
      part1 = maximum $ [area p1 p2 | p1 <- pts, p2 <- pts, p1 < p2]
  print part1

-- any more efficient way to go from [(a,b)] -> ([a], [b])?
-- xs = map fst pts
-- ys = map snd pts
-- print $ length pts
-- print (minimum xs, maximum xs)
-- print (minimum ys, maximum ys)
-- print (length $ nub xs)
-- print (length $ nub ys)
