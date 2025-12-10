-- https://adventofcode.com/2025/day/10

import AocLib
import Data.List.Split
import System.Environment (getArgs)

data Machine = Machine {target :: Int, buttons :: [Int], joltages :: [Int]} deriving (Show, Eq)

parseDiagram :: String -> Int
parseDiagram [] = 0
parseDiagram ('.' : xs) = 2 * parseDiagram xs
parseDiagram ('#' : xs) = 1 + 2 * parseDiagram xs
parseDiagram s = error $ "Invalid wiring diagram: " ++ s

parseButton :: String -> Int
parseButton s = go (splitOn "," s)
  where
    go [] = 0
    go (x : xs) = 2 ^ loudRead @Int x + go xs

parseLine :: String -> Machine
parseLine str = case words (eraseChars "[](){}" str) of
  (target : rest) ->
    Machine
      { target = parseDiagram target,
        buttons = map parseButton $ init rest,
        joltages = map (loudRead @Int) $ splitOn "," $ last rest
      }
  _ -> error $ "Unable to parse " ++ str

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let x = map parseLine $ lines content
  print x
