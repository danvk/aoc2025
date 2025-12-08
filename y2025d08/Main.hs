-- https://adventofcode.com/2025/day/8

import AocLib
import Data.Bifunctor
import Data.Function
import Data.List
import Data.List.Split
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
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

fromEdges :: (Ord a) => [(a, a)] -> M.Map a [a]
fromEdges pairs = M.fromList edges
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

componentSize :: [(Int, Int)] -> Int
componentSize = length . head . connectedComponents . fromEdges

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
      numPoints = (read @Int) (args !! 1)
  content <- readFile inputFile
  let pts = zip [0 :: Int ..] $ map parsePoint $ lines content
      totalPoints = length pts
      pairs = map snd $ sort [(dist p1 p2, (i, j)) | (i, p1) <- pts, (j, p2) <- pts, i < j]
      comps1 = connectedComponents $ fromEdges $ take numPoints pairs
      biggest = take 3 $ sortBy (flip compare `on` length) comps1
      part1 = product $ map length biggest
      part2idx = fromJust $ lowerBound (\n -> componentSize (take n pairs) `compare` totalPoints) (1, length pairs)
      ((_, (x1, _, _)), (_, (x2, _, _))) = bimap (pts !!) (pts !!) $ pairs !! (part2idx - 1)
      part2 = x1 * x2
  print part1
  print part2
