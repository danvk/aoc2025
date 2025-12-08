module Grid (Point, Size, Grid, parseGrid, gridToStr, charAtPoint, neighbors) where

import Data.List
import Data.Map.Strict qualified as M

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

charAtPoint :: Grid -> Point -> Char
charAtPoint g pt = M.findWithDefault '.' pt g

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
