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
      rows1 = take 40 $ iterate nextRow line1
      part1 = length $ concatMap (filter ('.' ==)) rows1
      rows2 = take 400000 $ iterate nextRow line1
      part2 = length $ concatMap (filter ('.' ==)) rows2
  print part1
  print part2
