-- https://adventofcode.com/2025/day/4

import Data.List
import Data.Map qualified as M
import System.Environment (getArgs)

type Point = (Int, Int)

type Size = (Int, Int)

type Grid = M.Map Point Char

parseGrid :: String -> (Size, Grid)
parseGrid str = ((w, h), grid)
  where
    rows = lines str
    h = length rows
    w = length $ head rows
    grid = M.fromList [((x, y), c) | (y, row) <- zip [0 ..] rows, (x, c) <- zip [0 ..] row]

gridToStr :: Size -> Grid -> String
gridToStr (w, h) g = intercalate "\n" $ map rowToStr [0 .. (h - 1)]
  where
    rowToStr y = [M.findWithDefault '.' (x, y) g | x <- [0 .. (w - 1)]]

neighbors :: Size -> Point -> [Point]
neighbors (w, h) (x, y) =
  [ (x', y')
    | x' <- [x - 1 .. x + 1],
      x' >= 0,
      x' < w,
      y' <- [y - 1 .. y + 1],
      y' >= 0,
      y' < h,
      (x', y') /= (x, y)
  ]

numNeighbors :: Size -> Grid -> Point -> Int
numNeighbors dims g pt = sum $ map (\n -> if M.findWithDefault '.' n g == '@' then 1 else 0) (neighbors dims pt)

count1 :: Size -> Grid -> Int
count1 dims@(w, h) g =
  length
    [ True
      | x <- [0 .. w - 1],
        y <- [0 .. h - 1],
        M.findWithDefault '.' (x, y) g == '@'
          && numNeighbors dims g (x, y) < 4
    ]

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let (dims, initState) = parseGrid content
  -- putStr $ gridToStr dims initState
  print $ count1 dims initState
