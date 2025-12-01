-- https://adventofcode.com/2015/day/13

import Circuit
import System.Environment (getArgs)

-- Carol would lose 62 happiness units by sitting next to Alice.
-- Carol would gain 60 happiness units by sitting next to Bob.
parseLine :: String -> Triple
parseLine line = case words line of
  [p1, "would", "lose", units, "happiness", "units", "by", "sitting", "next", "to", p2] -> (p1, filter (/= '.') p2, (-1) * read units)
  [p1, "would", "gain", units, "happiness", "units", "by", "sitting", "next", "to", p2] -> (p1, filter (/= '.') p2, read units)
  _ -> error $ "Invalid line: " ++ line

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let rawTriples = map parseLine $ lines content
      part1 = findMinCircuit rawTriples True
      part2 = findMinCircuit rawTriples False
  print part1
  print part2
