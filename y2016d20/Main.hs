-- https://adventofcode.com/2016/day/20

import Data.List
import Data.List.Split
import System.Environment (getArgs)

parseLine :: String -> (Int, Int)
parseLine str = case splitOn "-" str of
  [a, b] -> (read a, read b)
  _ -> error $ "Unable to parse " ++ str

shrinkRanges :: [(Int, Int)] -> [(Int, Int)]
shrinkRanges [] = []
shrinkRanges (r : rs) = go r rs
  where
    go r1 [] = [r1]
    go r1@(x1, y1) ((x2, y2) : xs) =
      if x2 > y1 + 1
        then
          r1 : go (x2, y2) xs
        else
          go (x1, max y1 y2) xs

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let ranges = sort $ map parseLine $ lines content
      mergedRanges = shrinkRanges ranges
      part1 = 1 + snd (head mergedRanges)
      included = zip (map snd mergedRanges) (tail $ map fst mergedRanges)
      part2 = sum $ map (\(a, b) -> b - a - 1) included
  print part1
  print part2

-- print $ head mergedRanges
-- print $ last mergedRanges
-- print $ take 3 mergedRanges
-- print $ take 3 included
-- print $ reverse $ take 3 $ reverse mergedRanges
-- print $ last included