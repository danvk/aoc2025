-- https://adventofcode.com/2015/day/16

import Data.List.Split
import Data.Map qualified as M
import System.Environment (getArgs)
import Text.Read

target :: M.Map String Integer
target =
  M.fromList
    [ ("children", 3),
      ("cats", 7),
      ("samoyeds", 2),
      ("pomeranians", 3),
      ("akitas", 0),
      ("vizslas", 0),
      ("goldfish", 5),
      ("trees", 3),
      ("cars", 2),
      ("perfumes", 1)
    ]

loudRead :: (Read a) => String -> a
loudRead s = case readMaybe s of
  Just x -> x
  Nothing -> error $ "Unable to parse '" ++ s ++ "'"

eraseChars :: String -> String -> String
eraseChars elim = filter (not . (`elem` elim))

-- Sue 1: children: 1, cars: 8, vizslas: 7
parseSue :: String -> (Int, M.Map String Integer)
parseSue str = case words (eraseChars ":," str) of
  "Sue" : n : compounds -> (read n, readMap compounds)
  _ -> error $ "Invalid Sue " ++ str
  where
    readPair [comp, amt] = (comp, loudRead amt)
    readPair x = error $ "expected pair: " ++ show x
    readMap s = M.fromList $ map readPair $ chunksOf 2 s

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let x = map parseSue $ lines content
  print $ take 10 x
