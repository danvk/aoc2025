-- https://adventofcode.com/2025/day/6
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Char
import Data.List
import Data.List.Split
import System.Environment (getArgs)

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

applyOp :: [String] -> Int
applyOp ("*" : nums) = product $ map (read @Int) nums
applyOp ("+" : nums) = sum $ map (read @Int) nums

transform2 :: [String] -> [String]
transform2 (op : nums) = trim op : map reverse (transpose nums)

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let part1 = sum $ map (applyOp . reverse) (transpose $ map words $ lines content)
      part2 = sum $ map (applyOp . transform2 . reverse . transpose) (wordsBy (all (== ' ')) $ transpose $ lines content)
  print part1
  print part2
