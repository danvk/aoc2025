-- https://adventofcode.com/2025/day/3

import Data.Char
import System.Environment (getArgs)

chooseWithMax :: Int -> [Int] -> [Int]
chooseWithMax 0 _ = []
chooseWithMax n xs = best : chooseWithMax (n - 1) rest
  where
    best = maximum $ take (length xs - n + 1) xs
    (_, _ : rest) = break (== best) xs

maxJoltage :: Int -> [Int] -> Int
maxJoltage n xs = foldl1 (\acc x -> 10 * acc + x) $ chooseWithMax n xs

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let nums = map (map digitToInt) $ lines content
      part1 = sum $ map (maxJoltage 2) nums
      part2 = sum $ map (maxJoltage 12) nums
  print part1
  print part2
