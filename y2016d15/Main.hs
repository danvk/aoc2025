-- https://adventofcode.com/2016/day/15

import AocLib (eraseChars)
import Data.List
import System.Environment (getArgs)

-- Disc #1 has 5 positions; at time=0, it is at position 4.
-- Disc #2 has 2 positions; at time=0, it is at position 1.

-- d1 = 4 + t (mod 5)
-- d2 = 1 + t (mod 2)
-- want:
-- d1(T+1) = 0
-- d2(T+2) = 0

parseLine :: String -> (Int, Int, Int)
parseLine str = case words (eraseChars ".#" str) of
  ["Disc", x, "has", n, "positions;", "at", "time=0,", "it", "is", "at", "position", p] ->
    (read x, read n, read p)
  _ -> error $ "Unable to parse " ++ show (words str)

isSolution :: Int -> (Int, Int, Int) -> Bool
isSolution t0 (discNum, numPos, p0) = (p0 + discNum + t0) `mod` numPos == 0

isBigSolution :: Int -> [(Int, Int, Int)] -> Bool
isBigSolution t0 = all (isSolution t0)

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let discs = map parseLine $ lines content
      maxT = product $ map (\(_, n, _) -> n) discs
      maxDisc = maximum $ map (\(n, _, _) -> n) discs
      newDisc = (1 + maxDisc, 11, 0)
      discs2 = discs ++ [newDisc]
  print newDisc
  print $ find (`isBigSolution` discs) [0 .. maxT]
  print $ find (`isBigSolution` discs2) [0 .. maxT * 11]
