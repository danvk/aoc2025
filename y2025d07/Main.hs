-- https://adventofcode.com/2025/day/7

import Data.List
import Data.Map.Strict qualified as M
import Grid
import System.Environment (getArgs)

findChar :: Grid -> Char -> Point
findChar g c = case find ((==) c . snd) $ M.toList g of
  Nothing -> error $ "Unable to find " ++ [c]
  Just (p, _) -> p

stepBeam :: Grid -> Point -> (Int, [Point])
stepBeam g (x, y) = case charAtPoint g (x, y + 1) of
  '.' -> (0, [(x, y + 1)])
  '^' -> (1, [(x - 1, y + 1), (x + 1, y + 1)])
  c -> error $ "Surprise char in grid: " ++ [c]

step :: Grid -> (Int, [Point]) -> (Int, [Point])
step g (splits, beams) = (splits + new_splits, new_beams)
  where
    nexts = map (stepBeam g) beams
    new_beams = nub $ concatMap snd nexts
    new_splits = sum $ map fst nexts

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let (dims@(_, height), g) = parseGrid content
      start = findChar g 'S'
  -- putStrLn $ gridToStr dims g
  -- print start
  let (part1, _) = iterate (step g) (0, [start]) !! height
  print part1

-- print $ stepBeam g (7, 1)
