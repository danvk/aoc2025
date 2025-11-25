#!/usr/bin/env cabal
{- cabal:
build-depends:
  base,
  split ^>=0.2.5
-}
-- https://adventofcode.com/2015/day/2
import System.Environment (getArgs)
import Data.List
import Data.List.Split

parseLine :: String -> (Int, Int, Int)
parseLine line =
    case parts of
        [a, b, c] -> (read a, read b, read c)
    where parts = splitOn "x" line

sides :: (Int, Int, Int) -> [Int]
sides (l, w, h) = [l*w, w*h, h*l]

wrappingPaper :: (Int, Int, Int) -> Int
wrappingPaper dims = 2 * sum (sides dims) + minimum (sides dims)

volume (l, w, h) = l*w*h

tripleToList :: (a, a, a) -> [a]
tripleToList (x, y, z) = [x, y, z]

ribbon :: (Int, Int, Int) -> Int
ribbon dims =
    2*side + volume dims
    where side = sum $ take 2 (sort $ tripleToList dims)

main = do
    args <- getArgs
    let inputFile = head args
    content <- readFile inputFile
    let presents = map parseLine $ lines content
    let squareFeet = map wrappingPaper presents
    let part1 = sum squareFeet
    print part1
    let part2 = sum $ map ribbon presents
    print part2
