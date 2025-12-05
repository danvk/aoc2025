-- https://adventofcode.com/2015/day/20

import Data.List
import System.Environment (getArgs)

intSqrt :: Int -> Int
intSqrt n = floor (sqrt (fromIntegral n :: Double))

presents :: Int -> Int
presents n = 10 * sum (map (\x -> n `div` x + x) (filter (\x -> n `mod` x == 0) [1 .. sqrtn])) - overcount
  where
    sqrtn = intSqrt n
    overcount = if sqrtn * sqrtn == n then 10 * sqrtn else 0

main :: IO ()
main = do
  args <- getArgs
  let n = read @Int $ head args
      houses = zip [1 :: Int ..] $ map presents [1 ..]
      match = find (\(_, num) -> num >= n) houses
  print match
