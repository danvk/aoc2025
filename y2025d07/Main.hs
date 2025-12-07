-- https://adventofcode.com/2025/day/7

import Data.Bifunctor qualified
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

-- TODO: understand this
mapFsts :: (a -> b) -> [(a, c)] -> [(b, c)]
mapFsts f = map (Data.Bifunctor.first f)

-- TODO: there must be a more idiomatic way to write this
step2 :: Grid -> [(Point, Int)] -> [(Point, Int)]
step2 g beams = merged_beams
  where
    nexts = mapFsts (snd . stepBeam g) beams
    new_beams = concatMap (\(pts, count) -> map (,count) pts) nexts
    grouped_beams = groupBy (\a b -> fst a == fst b) $ sort new_beams
    merged_beams = map (\pcs -> (fst $ head pcs, sum $ map snd pcs)) grouped_beams

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
  let part2 = sum $ map snd $ iterate (step2 g) [(start, 1)] !! height
  print part2

-- print $ stepBeam g (7, 1)
