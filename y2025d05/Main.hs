-- https://adventofcode.com/2025/day/5
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List.Split
import System.Environment (getArgs)

parseRange :: String -> (Int, Int)
parseRange str =
  let [left, right] = map (read @Int) $ splitOn "-" str
   in (left, right)

inRange :: (Int, Int) -> Int -> Bool
inRange (a, b) x = x >= a && x <= b

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
  print part1

-- print freshRanges
-- print ingredients
