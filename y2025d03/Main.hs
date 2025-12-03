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

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let part1 = sum $ map (max1 . parseLine) (lines content)
  print part1
