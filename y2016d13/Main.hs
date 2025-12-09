-- https://adventofcode.com/2016/day/13
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.Bits
import Data.Heap qualified
import Data.List
import Data.Set qualified as S
import Grid
import System.Environment (getArgs)

isWall :: Int -> (Int, Int) -> Bool
isWall seed (x, y)
  | x < 0 || y < 0 = True
  | otherwise = odd $ popCount (seed + x * x + 3 * x + 2 * x * y + y + y * y)

bfs :: (Ord a) => (a -> [a]) -> (a -> Int) -> (a -> Bool) -> [a] -> Maybe a
bfs stepFn weight done starts = go initHeap S.empty
  where
    initList = zip (map weight starts) starts
    initHeap = Data.Heap.fromList initList `asTypeOf` (undefined :: Data.Heap.MinPrioHeap Int a)
    go h visited = case Data.Heap.view h of
      Just ((_, val), rest) ->
        if val `S.member` visited
          then go rest visited
          else
            if done val then Just val else go (insertAll rest $ map (\x -> (weight x, x)) (stepFn val)) (S.insert val visited)
      Nothing -> Nothing
    insertAll = foldr Data.Heap.insert

bfsToNum :: (Ord a) => (a -> [a]) -> (a -> Int) -> Int -> [a] -> [a]
bfsToNum stepFn weight maxCost starts = go initHeap S.empty
  where
    initList = zip (map weight starts) starts
    initHeap = Data.Heap.fromList initList `asTypeOf` (undefined :: Data.Heap.MinPrioHeap Int a)
    go h visited = case Data.Heap.view h of
      Just ((d, val), rest) ->
        if d > maxCost
          then []
          else
            if val `S.member` visited
              then go rest visited
              else
                val : go (insertAll rest $ map (\x -> (weight x, x)) (stepFn val)) (S.insert val visited)
      Nothing -> []
    insertAll = foldr Data.Heap.insert

stepPt :: Int -> (Int, Int) -> [(Int, Int)]
stepPt seed = filter (not . isWall seed) . neighbors4

stepD :: (a -> [a]) -> ((Int, a) -> [(Int, a)])
stepD stepFn = fn
  where
    fn (n, pt) = map (n + 1,) $ stepFn pt

main :: IO ()
main = do
  args <- getArgs
  let [seed, tx, ty, maxSteps] = map (read @Int) args
      start = (1, 1)
      target = (tx, ty)
      step = stepD (stepPt seed)
      part1 = bfs step fst (\(_, pt) -> pt == target) [(0, start)]
      part2 = length $ nub $ map snd $ bfsToNum step fst maxSteps [(0, start)]
  putStrLn $ intercalate "\n" $ map (\y -> map (\x -> if isWall seed (x, y) then '#' else '.') [0 .. 9]) [0 .. 6]
  print part1
  print part2
