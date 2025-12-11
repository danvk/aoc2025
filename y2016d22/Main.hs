-- https://adventofcode.com/2016/day/22

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

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let x = map parseLine $ drop 2 $ lines content
  print x
