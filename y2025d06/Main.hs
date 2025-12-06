-- https://adventofcode.com/2025/day/6
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.List
import System.Environment (getArgs)

applyOp :: [String] -> Int
applyOp ("*" : nums) = product $ map (read @Int) nums
applyOp ("+" : nums) = sum $ map (read @Int) nums

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let part1 = sum $ map (applyOp . reverse) (transpose $ map words $ lines content)
  print part1
