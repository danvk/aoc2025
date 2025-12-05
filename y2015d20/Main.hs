-- https://adventofcode.com/2015/day/20

import Data.List
import System.Environment (getArgs)

elf :: Int -> [Int]
elf n = intercalate zeros [[10 * x] | x <- [1 ..]]
  where
    zeros = replicate (n - 1) 0

elves :: Int -> [Int]
elves n = head elfn : zipWith (+) (tail elfn) elvesn1
  where
    elfn = elf n
    elvesn1 = elves (n + 1)

main :: IO ()
main = do
  args <- getArgs
  let n = read @Int $ head args
      houses = zip [1 :: Int ..] $ elves 1
      match = find (\(_, num) -> num >= n) houses
  print match
