-- https://adventofcode.com/2015/day/16

import AocLib
import Data.List.Split
import Data.Map qualified as M
import System.Environment (getArgs)

type Compounds = M.Map String Integer

target :: Compounds
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

target2 :: M.Map String (Integer -> Bool)
target2 =
  M.fromList
    [ ("children", (== 3)),
      ("samoyeds", (== 2)),
      ("akitas", (== 0)),
      ("vizslas", (== 0)),
      ("cars", (== 2)),
      ("perfumes", (== 1)),
      ("cats", (> 7)),
      ("trees", (> 3)),
      ("goldfish", (< 5)),
      ("pomeranians", (< 3))
    ]

-- Sue 1: children: 1, cars: 8, vizslas: 7
parseSue :: String -> (Int, Compounds)
parseSue str = case words (eraseChars ":," str) of
  "Sue" : n : compounds -> (read n, readMap compounds)
  _ -> error $ "Invalid Sue " ++ str
  where
    readPair [comp, amt] = (comp, loudRead amt)
    readPair x = error $ "expected pair: " ++ show x
    readMap s = M.fromList $ map readPair $ chunksOf 2 s

isValidSue :: Compounds -> Bool
isValidSue compounds = and [target M.! key == count | (key, count) <- M.toList compounds]

isValidSue2 :: Compounds -> Bool
isValidSue2 compounds = and [(target2 M.! key) count | (key, count) <- M.toList compounds]

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let sues = map parseSue $ lines content
      part1 = filter (isValidSue . snd) sues
      part2 = filter (isValidSue2 . snd) sues
  print part1
  print part2
