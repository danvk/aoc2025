-- https://adventofcode.com/2025/day/5
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List
import Data.List.Split
import System.Environment (getArgs)

parseRange :: String -> (Int, Int)
parseRange str =
  let [left, right] = map (read @Int) $ splitOn "-" str
   in (left, right + 1)

inRange :: (Int, Int) -> Int -> Bool
inRange (a, b) x = x >= a && x < b

inRangeInc :: (Int, Int) -> Int -> Bool
inRangeInc (a, b) x = x >= a && x <= b

splitRange :: [Int] -> (Int, Int) -> [(Int, Int)]
splitRange edges r = zip pts (tail pts)
  where
    pts = sort $ filter (r `inRangeInc`) edges

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let inLines = lines content
      [ranges, ingredientStrs] = splitOn [""] inLines
      freshRanges = map parseRange ranges
      ingredients = map (read @Int) ingredientStrs
      freshIngredients = filter (\x -> any (`inRange` x) freshRanges) ingredients
      part1 = length freshIngredients
      edges = nub $ concatMap (\(a, b) -> [a, b]) freshRanges
      disjointRanges = nub $ sort $ concatMap (splitRange edges) freshRanges
      part2 = sum $ map (\(a, b) -> b - a) disjointRanges
  print part1
  print part2
