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
choose n (x : xs) = takeIt ++ dontTakeIt
  where
    takeIt = map (x :) $ choose (n - 1) xs
    dontTakeIt = choose n xs

max1 :: [Int] -> Int
max1 xs = maximum $ map sum2 $ choose 2 xs
  where
    sum2 [a, b] = 10 * a + b

chooseWithMax :: Int -> [Int] -> [Int]
chooseWithMax 0 _ = []
chooseWithMax n xs = best : chooseWithMax (n - 1) rest
  where
    best = maximum $ take (length xs - n + 1) xs
    (_, _ : rest) = break (== best) xs

max2 :: [Int] -> Int
max2 xs = foldl1 (\acc x -> 10 * acc + x) $ chooseWithMax 12 xs

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
