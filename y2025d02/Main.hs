-- https://adventofcode.com/2025/day/2

import Data.List.Split
import System.Environment (getArgs)

toPair :: (Show a) => [a] -> (a, a)
toPair [a, b] = (a, b)
toPair x = error $ "Expected two elements, got " ++ show x

parseRanges :: String -> [(Int, Int)]
parseRanges txt = map parseRange $ splitOn "," txt
  where
    parseRange r = toPair $ map read $ splitOn "-" r

isInvalidNumber :: Int -> Bool
isInvalidNumber num = left == right
  where
    str = show num
    (left, right) = splitAt (length str `div` 2) str

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let pairs = parseRanges content
      counts = map (\(a, b) -> b - a) pairs
      part1 = sum $ pairs >>= (\(a, b) -> filter isInvalidNumber [a .. b])
  print pairs
  print counts
  print part1
