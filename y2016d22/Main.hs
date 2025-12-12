-- https://adventofcode.com/2016/day/22
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import AocLib
import Data.Heap qualified
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Set qualified as S
import Grid
import System.Environment (getArgs)

data Disk = Disk
  { pos :: (Int, Int),
    size :: Int,
    used :: Int,
    avail :: Int,
    usePct :: Int
  }
  deriving (Show, Eq)

-- Filesystem              Size  Used  Avail  Use%
-- /dev/grid/node-x0-y0     94T   73T    21T   77%
parseLine :: String -> Disk
parseLine str = case words (eraseChars "%T" str) of
  [disk, size, used, avail, usePct] ->
    let ["/dev/grid/node", 'x' : x, 'y' : y] = splitOn "-" disk
     in Disk
          { pos = (loudRead x, loudRead y),
            size = loudRead size,
            used = loudRead used,
            avail = loudRead avail,
            usePct = loudRead usePct
          }
  _ -> error $ "Unable to parse " ++ str

isViable :: Disk -> Disk -> Bool
isViable a b = (used a /= 0) && (a /= b) && (used a <= avail b)

-- Position of goal, position of open space
type State = (Point, Point)

-- can move the open pos in any direction
-- if this would collide with the goal pos, swap them.
step :: Size -> State -> [State]
step (w, h) (goalPos, openPos) =
  [ if (x, y) /= goalPos then (goalPos, (x, y)) else (openPos, goalPos)
    | (x, y) <- neighbors4 openPos,
      x >= 0 && x < w && y >= 0 && y < h
  ]

stepD :: (a -> [a]) -> ((Int, a) -> [(Int, a)])
stepD stepFn = fn
  where
    fn (n, pt) = map (n + 1,) $ stepFn pt

isDone :: (Int, State) -> Bool
isDone (_, ((0, 0), _)) = True
isDone _ = False

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

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let disks = map parseLine $ drop 2 $ lines content
      part1 = length [() | a <- disks, b <- disks, isViable a b]
  print part1
  let openPos = pos $ fromJust $ find (\d -> used d == 0) disks
      goalPos = maximum [pos d | d <- disks, (snd . pos) d == 0]
      (mx, my) = maximum $ map pos disks
      dims = (1 + mx, 1 + my)
      state0 = (goalPos, openPos)
      part2 = bfs (stepD (step dims)) fst isDone [(0, state0)]
  -- part2 = step dims state0
  -- part22 = step dims ((2, 0), (1, 0))
  -- print disks
  print dims
  print state0
  print part2

-- print ((2, 0), (1, 0))
-- print part22
