-- https://adventofcode.com/2025/day/7

import Data.List
import Data.Map.Strict qualified as M
import Grid
import System.Environment (getArgs)

findChar :: Grid -> Char -> Point
findChar g c = case find ((==) c . snd) $ M.toList g of
  Nothing -> error $ "Unable to find " ++ [c]
  Just (p, _) -> p

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let (dims, g) = parseGrid content
  putStrLn $ gridToStr dims g
  print $ findChar g 'S'
