-- https://adventofcode.com/2025/day/3

import Data.Char
import System.Environment (getArgs)

parseLine :: String -> [Int]
parseLine = map digitToInt

choose :: Int -> [a] -> [[a]]
choose 0 [] = [[]]
choose 0 _ = []
choose _ [] = []
choose 1 xs = [[x] | x <- xs]
choose n lst@(x : xs)
  | n < length lst = []
  | n == length lst = [xs]
  | otherwise = takeIt ++ dontTakeIt
  where
    takeIt = map (x :) $ choose (n - 1) xs
    dontTakeIt = choose n xs

max1 :: [Int] -> Int
max1 xs = maximum $ map sum2 $ choose 2 xs
  where
    sum2 [a, b] = 10 * a + b

max2 :: [Int] -> Int
max2 xs = maximum $ map sum12 $ choose 12 xs
  where
    sum12 = foldl1 (\acc x -> 10 * acc + x)

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let part1 = sum $ map (max1 . parseLine) (lines content)
  print part1
  let part2s = map (max2 . parseLine) (lines content)
  print part2s
  print $ sum part2s
