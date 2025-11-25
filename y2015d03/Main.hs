#!/usr/bin/env cabal
{- cabal:
build-depends:
  base,
  split ^>=0.2.5
-}
-- https://adventofcode.com/2015/day/3
import System.Environment (getArgs)

toDir :: Char -> (Int, Int)
toDir '<' = (-1, 0)
toDir '>' = (1, 0)
toDir '^' = (0, -1)
toDir 'v' = (0, 1)
toDir c = error ("Invalid character '" ++ [c] ++ "'")

-- TODO: probably a better way to write this
move :: (Int, Int) -> (Int, Int) -> (Int, Int)
move (a, b) (c, d) = (a+c, b+d)

main = do
    args <- getArgs
    let inputFile = head args
    content <- readFile inputFile
    let dirs = map toDir (head (lines content))
    let coords = scanl move (0, 0) dirs
    print coords
