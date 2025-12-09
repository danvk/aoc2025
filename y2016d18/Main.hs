-- https://adventofcode.com/2016/day/18
import System.Environment (getArgs)

nextChar :: Char -> Char -> Char -> Char
nextChar '^' '^' '.' = '^'
nextChar '.' '^' '^' = '^'
nextChar '^' '.' '.' = '^'
nextChar '.' '.' '^' = '^'
nextChar _ _ _ = '.'

nextRow :: String -> String
nextRow s = zipWith3 nextChar ('.' : s) s (tail s ++ ['.'])

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let line1 = head $ lines content
      n = 40
      rows = take n $ iterate nextRow line1
      part1 = length $ concatMap (filter ('.' ==)) rows
  -- print rows
  print part1
