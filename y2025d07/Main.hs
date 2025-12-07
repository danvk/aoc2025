-- https://adventofcode.com/2025/day/7
import Grid
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let (dims, g) = parseGrid content
  putStrLn $ gridToStr dims g
