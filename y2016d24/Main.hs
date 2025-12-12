-- https://adventofcode.com/2016/day/24

import Data.Char (digitToInt)
import Data.Map.Strict qualified as M
import GHC.Unicode
import Grid
import System.Environment (getArgs)

findNums :: Grid -> M.Map Int Point
findNums g = M.fromList [(digitToInt c, (x, y)) | ((x, y), c) <- M.toList g, isDigit c]

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let (dims, g) = parseGrid content
  print dims
  print $ findNums g
