-- https://adventofcode.com/2015/day/18

import Data.List
import Data.Map qualified as M
import System.Environment (getArgs)

type Point = (Int, Int)

type Size = (Int, Int)

type Grid = M.Map Point Bool

parseGrid :: String -> (Size, Grid)
parseGrid str = ((w, h), grid)
  where
    rows = lines str
    h = length rows
    w = length $ head rows
    grid = M.fromList [((x, y), c == '#') | (y, row) <- zip [0 ..] rows, (x, c) <- zip [0 ..] row]

gridToStr :: Size -> Grid -> String
gridToStr (w, h) g = intercalate "\n" $ map rowToStr [0 .. (h - 1)]
  where
    rowToStr y = [if M.findWithDefault False (x, y) g then '#' else '.' | x <- [0 .. (w - 1)]]

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
numNeighbors dims g pt = sum $ map (\n -> if M.findWithDefault False n g then 1 else 0) (neighbors dims pt)

nextState :: Bool -> Int -> Bool
nextState True 2 = True
nextState True 3 = True
nextState False 3 = True
nextState _ _ = False

step :: Size -> Grid -> Grid
step dims@(w, h) g =
  M.fromList
    [ ( (x, y),
        nextState (M.findWithDefault False (x, y) g) (numNeighbors dims g (x, y))
      )
      | x <- [0 .. (w - 1)],
        y <- [0 .. (h - 1)]
    ]

numAlive :: Grid -> Int
numAlive g = length $ filter id $ M.elems g

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let (dims, initState) = parseGrid content
      counts = map numAlive $ iterate (step dims) initState
      part1 = counts !! 100
  print part1
