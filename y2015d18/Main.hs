-- https://adventofcode.com/2015/day/18

import Data.Map qualified as M
import Grid
import System.Environment (getArgs)

numNeighbors :: Size -> Grid -> Point -> Int
numNeighbors dims g pt = length $ filter ((== '#') . charAtPoint g) (neighbors dims pt)

nextState :: Char -> Int -> Char
nextState '#' 2 = '#'
nextState '#' 3 = '#'
nextState '.' 3 = '#'
nextState _ _ = '.'

step :: Size -> Grid -> Grid
step dims@(w, h) g =
  M.fromList
    [ ( (x, y),
        nextState (charAtPoint g (x, y)) (numNeighbors dims g (x, y))
      )
      | x <- [0 .. (w - 1)],
        y <- [0 .. (h - 1)]
    ]

jamCorners :: Size -> Grid -> Grid
jamCorners (w, h) g =
  M.fromList
    ( M.toList g
        ++ [ ((0, 0), '#'),
             ((w - 1, 0), '#'),
             ((0, h - 1), '#'),
             ((w - 1, h - 1), '#')
           ]
    )

step2 :: Size -> Grid -> Grid
step2 dims g = jamCorners dims (step dims g)

numAlive :: Grid -> Int
numAlive g = length $ filter (== '#') $ M.elems g

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let (dims, initState) = parseGrid content
      counts = map numAlive $ iterate (step dims) initState
      part1 = counts !! 100
      counts2 = map numAlive $ iterate (step2 dims) (jamCorners dims initState)
      part2 = counts2 !! 100
  print part1
  print part2
