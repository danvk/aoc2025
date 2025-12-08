-- https://adventofcode.com/2025/day/8

import AocLib
import Data.List
import Data.List.Split
import Data.Map.Strict qualified as M
import Data.Tuple
import System.Environment (getArgs)

type Point = (Int, Int, Int)

dist :: Point -> Point -> Int
dist (x1, y1, z1) (x2, y2, z2) = (x1 - x2) ^ (2 :: Int) + (y1 - y2) ^ (2 :: Int) + (z1 - z2) ^ (2 :: Int)

parsePoint :: String -> Point
parsePoint str = case splitOn "," str of
  [x, y, z] -> (read x, read y, read z)
  _ -> error $ "Unable to parse " ++ str

toEdges :: (Ord a) => [(a, a)] -> M.Map a [a]
toEdges pairs = M.fromList edges
  where
    edges = mapReduce (\ab -> [ab, swap ab]) (\_ b -> b) pairs

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
      numPoints = (read @Int) (args !! 1)
  content <- readFile inputFile
  let pts = zip [0 ..] $ map parsePoint $ lines content
      ds = take numPoints $ map snd $ sort [(dist p1 p2, (i, j)) | (i, p1) <- pts, (j, p2) <- pts, i < j]
      g = toEdges ds
  print g
