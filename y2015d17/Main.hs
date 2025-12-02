-- https://adventofcode.com/2015/day/17
import System.Environment (getArgs)

waysToSum :: Int -> [Int] -> [[Int]]
waysToSum 0 [] = [[]]
waysToSum _ [] = []
waysToSum n (x : xs) = takeIt ++ dontTakeIt
  where
    takeIt = if x <= n then map (x :) $ waysToSum (n - x) xs else []
    dontTakeIt = waysToSum n xs

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
      target = (read @Int) (args !! 1)
  content <- readFile inputFile
  let nums = map (read @Int) $ lines content
      part1 = length $ waysToSum target nums
  print nums
  print target
  print part1
