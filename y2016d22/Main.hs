-- https://adventofcode.com/2016/day/22
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import AocLib
import Data.List.Split
import System.Environment (getArgs)

data Disk = Disk
  { pos :: (Int, Int),
    size :: Int,
    used :: Int,
    avail :: Int,
    usePct :: Int
  }
  deriving (Show, Eq)

-- Filesystem              Size  Used  Avail  Use%
-- /dev/grid/node-x0-y0     94T   73T    21T   77%
parseLine :: String -> Disk
parseLine str = case words (eraseChars "%T" str) of
  [disk, size, used, avail, usePct] ->
    let ["/dev/grid/node", 'x' : x, 'y' : y] = splitOn "-" disk
     in Disk
          { pos = (loudRead x, loudRead y),
            size = loudRead size,
            used = loudRead used,
            avail = loudRead avail,
            usePct = loudRead usePct
          }
  _ -> error $ "Unable to parse " ++ str

isViable :: Disk -> Disk -> Bool
isViable a b = (used a /= 0) && (a /= b) && (used a <= avail b)

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let disks = map parseLine $ drop 2 $ lines content
      part1 = length [() | a <- disks, b <- disks, isViable a b]
  print part1
