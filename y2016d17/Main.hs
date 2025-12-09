-- https://adventofcode.com/2016/day/17

import Data.Heap qualified
import Data.Maybe
import Grid
import Md5Lib
import System.Environment (getArgs)

type State = (Point, String)

dirs :: [(Char, Point)]
dirs =
  [ ('U', (0, -1)),
    ('D', (0, 1)),
    ('L', (-1, 0)),
    ('R', (1, 0))
  ]

addPt :: Point -> Point -> Point
addPt (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

inBounds :: Point -> Bool
inBounds (x, y) = x >= 0 && x < 4 && y >= 0 && y < 4

step :: State -> [State]
step ((x, y), path) = nexts
  where
    openMoves = map snd $ filter (\(c, _) -> c `elem` "bcdef") $ zip (md5str path) dirs
    moved = [(addPt (x, y) c, path ++ [d]) | (d, c) <- openMoves]
    nexts = filter (inBounds . fst) moved

-- TODO: move this into lib
bfs :: (Ord a) => (a -> [a]) -> (a -> Int) -> (a -> Bool) -> [a] -> Maybe a
bfs stepFn weight done starts = go initHeap
  where
    initList = zip (map weight starts) starts
    initHeap = Data.Heap.fromList initList `asTypeOf` (undefined :: Data.Heap.MinPrioHeap Int a)
    go h = case Data.Heap.view h of
      Just ((_, val), rest) ->
        if done val then Just val else go (insertAll rest $ map (\x -> (weight x, x)) (stepFn val))
      Nothing -> Nothing
    insertAll = foldr Data.Heap.insert

main :: IO ()
main = do
  args <- getArgs
  let seed = head args
      state = ((0, 0), seed)
      soln = snd . fromJust $ bfs step (length . snd) ((==) (3, 3) . fst) [state]
      part1 = drop (length seed) soln
  print state
  print part1
