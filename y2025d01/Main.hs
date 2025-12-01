-- https://adventofcode.com/2025/day/1
import System.Environment (getArgs)

parseLine :: String -> Int
parseLine ('L' : xs) = -(read xs)
parseLine ('R' : xs) = read xs
parseLine line = error $ "Invalid line " ++ line

turn :: Int -> Int -> Int
turn a b = (a + b) `mod` 100

byOnes :: Int -> [Int]
byOnes x = replicate (abs x) (if x < 0 then -1 else 1)

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let rotations = map parseLine $ lines content
      positions = scanl turn 50 rotations
      part1 = length $ filter (== 0) positions
      rot1s = rotations >>= byOnes
      part2 = length $ filter (== 0) $ scanl turn 50 rot1s
  print part1
  print part2
