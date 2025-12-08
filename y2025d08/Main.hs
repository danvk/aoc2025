-- https://adventofcode.com/2025/day/8

import AocLib
import Data.List
import Data.List.Split
import Data.Map.Strict qualified as M
import Data.Set qualified as S
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

floodFill :: (Ord a) => (a -> [a]) -> [a] -> [a]
floodFill neighborFn starts = go starts S.empty
  where
    go [] _ = []
    go (x : xs) visited =
      if x `S.member` visited
        then
          go xs visited
        else
          x : go (neighborFn x ++ xs) (S.insert x visited)

connectedComponents :: (Ord a) => M.Map a [a] -> [[a]]
connectedComponents g = go nodes
  where
    nodes = M.keys g
    neighbors n = g M.! n
    go [] = []
    go (x : xs) = component : go remainder
      where
        component = floodFill neighbors [x]
        remainder = filter (`notElem` component) xs

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
      numPoints = (read @Int) (args !! 1)
  content <- readFile inputFile
  let pts = zip [0 :: Int ..] $ map parsePoint $ lines content
      ds = take numPoints $ map snd $ sort [(dist p1 p2, (i, j)) | (i, p1) <- pts, (j, p2) <- pts, i < j]
      g = toEdges ds
      comps = connectedComponents g
  print g
  print comps
