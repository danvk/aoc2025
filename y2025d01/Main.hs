-- https://adventofcode.com/2025/day/1
import System.Environment (getArgs)

parseLine :: String -> Int
parseLine ('L' : xs) = -(read xs)
parseLine ('R' : xs) = read xs
parseLine line = error $ "Invalid line " ++ line

turn :: Int -> Int -> Int
turn a b = (a + b) `mod` 100

clicks :: Int -> Int -> Int
clicks a b = length $ filter ((==) 0 . (`mod` 100)) (if a < b then [(a + 1) .. b] else [(a - 1), (a - 2) .. b])

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let rotations = map parseLine $ lines content
      positions = scanl turn 50 rotations
      part1 = length $ filter (== 0) positions
      abspos = scanl (+) 50 rotations
      part2 = sum $ zipWith clicks abspos (tail abspos)
  print part1
  print part2
