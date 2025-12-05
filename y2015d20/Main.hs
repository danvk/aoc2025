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

presents2 :: Int -> Int
presents2 n = 11 * sum [n `div` x | x <- [1 .. 50], n `mod` x == 0]

main :: IO ()
main = do
  args <- getArgs
  let n = read @Int $ head args
      houses = zip [1 :: Int ..] $ map presents [1 ..]
      match = find (\(_, num) -> num >= n) houses
      houses2 = zip [1 :: Int ..] $ map presents2 [1 ..]
      match2 = find (\(_, num) -> num >= n) houses2
  print match
  print match2
