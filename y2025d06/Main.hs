-- https://adventofcode.com/2025/day/6
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Char
import Data.List
import Data.List.Split
import System.Environment (getArgs)

applyOp :: [String] -> Int
applyOp ("*" : nums) = product $ map (read @Int) nums
applyOp ("+" : nums) = sum $ map (read @Int) nums

fn :: [String] -> [String]
fn (op : nums) = trim op : map reverse (transpose nums)

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let part1 = sum $ map (applyOp . reverse) (transpose $ map words $ lines content)
      part2 = sum $ map applyOp $ map (fn . reverse . transpose) $ wordsBy (all (== ' ')) $ transpose $ lines content
  print part1
  print part2
