-- https://adventofcode.com/2025/day/9

import Data.List
import Data.List.Split
import Data.Map.Strict qualified as M
import Grid
import System.Environment (getArgs)

parseLine :: String -> Point
parseLine str = case splitOn "," str of
  [a, b] -> (read a, read b)
  _ -> error $ "Unable to parse " ++ str

area :: Point -> Point -> Int
area (x1, y1) (x2, y2) = (x2 - x1 + 1) * (y2 - y1 + 1)

strokePath :: Grid -> [Point] -> Grid
strokePath initG pts = g'
  where
    pairs = zip pts $ tail pts ++ [head pts]
    stroke ((x1, y1), (x2, y2)) g
      | x1 == x2 = M.union g (M.fromList [((x1, y), 'X') | y <- [min y1 y2 + 1 .. max y1 y2 - 1]])
      | y1 == y2 = M.union g (M.fromList [((x, y1), 'X') | x <- [min x1 x2 + 1 .. max x1 x2 - 1]])
      | otherwise = error "Invalid pair"
    g' = foldr stroke initG pairs

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let pts = map parseLine $ lines content
      part1 = maximum $ [area p1 p2 | p1 <- pts, p2 <- pts, p1 < p2]
      (xs, ys) = unzip pts
      dims = (1 + maximum xs, 1 + maximum ys)
      g = M.fromList [(pt, '#') | pt <- pts]
      g' = strokePath g pts
  print part1
  putStrLn $ gridToStr dims g
  putStrLn ""
  putStrLn $ gridToStr dims g'

-- any more efficient way to go from [(a,b)] -> ([a], [b])?
-- xs = map fst pts
-- ys = map snd pts
-- (xs, ys) = unzip pts  -- <-- yes!
-- print $ length pts
-- print (minimum xs, maximum xs)
-- print (minimum ys, maximum ys)
-- print (length $ nub xs)
-- print (length $ nub ys)
