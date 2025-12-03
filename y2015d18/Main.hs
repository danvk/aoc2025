-- https://adventofcode.com/2015/day/18

import Data.List
import Data.Map qualified as M
import System.Environment (getArgs)

type Point = (Int, Int)

type Grid = M.Map Point Bool

parseGrid :: String -> (Int, Int, Grid)
parseGrid str = (w, h, grid)
  where
    rows = lines str
    h = length rows
    w = length $ head rows
    grid = M.fromList [((x, y), c == '#') | (y, row) <- zip [0 ..] rows, (x, c) <- zip [0 ..] row]

gridToStr :: Int -> Int -> Grid -> String
gridToStr w h g = intercalate "\n" $ map rowToStr [0 .. (h - 1)]
  where
    rowToStr y = [if M.findWithDefault False (x, y) g then '#' else '.' | x <- [0 .. (w - 1)]]

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let (w, h, initState) = parseGrid content
  print (w, h)
  putStrLn $ gridToStr w h initState
