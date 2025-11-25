#!/usr/bin/env cabal
{- cabal:
build-depends:
  base,
  split ^>=0.2.5
-}
-- https://adventofcode.com/2015/day/3
import System.Environment (getArgs)

main = do
    args <- getArgs
    let inputFile = head args
    content <- readFile inputFile
    print $ lines content
