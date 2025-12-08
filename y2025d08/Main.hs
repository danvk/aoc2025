-- https://adventofcode.com/2025/day/8

import Data.List
import Data.List.Split
import System.Environment (getArgs)

type Point = (Int, Int, Int)

dist :: Point -> Point -> Int
dist (x1, y1, z1) (x2, y2, z2) = (x1 - x2) ^ (2 :: Int) + (y1 - y2) ^ (2 :: Int) + (z1 - z2) ^ (2 :: Int)

parsePoint :: String -> Point
parsePoint str = case splitOn "," str of
  [x, y, z] -> (read x, read y, read z)
  _ -> error $ "Unable to parse " ++ str

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let pts = map parsePoint $ lines content
      ds = take 2 $ sort [(dist p1 p2, p1, p2) | p1 <- pts, p2 <- pts, p1 < p2]
  print ds
