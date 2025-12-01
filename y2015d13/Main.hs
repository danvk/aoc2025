-- https://adventofcode.com/2015/day/13

import Circuit
import Data.List
import System.Environment (getArgs)

-- Carol would lose 62 happiness units by sitting next to Alice.
-- Carol would gain 60 happiness units by sitting next to Bob.
parseLine :: String -> Triple
parseLine line = case words line of
  [p1, "would", "lose", units, "happiness", "units", "by", "sitting", "next", "to", p2] -> (p1, filter (/= '.') p2, (-1) * read units)
  [p1, "would", "gain", units, "happiness", "units", "by", "sitting", "next", "to", p2] -> (p1, filter (/= '.') p2, read units)
  _ -> error $ "Invalid line: " ++ line

first2 :: (a, b, c) -> (a, b)
first2 (a, b, _) = (a, b)

-- Sum (A, B, d1) and (B, A, d2) -> (A, B, d1+d2)
sumTriples :: [Triple] -> [Triple]
sumTriples pairs =
  let expanded = (pairs >>= (\p@(a, b, d) -> [p, (b, a, d)]))
   in let sorted = sort expanded
       in let grouped = groupBy (\a b -> first2 a == first2 b) sorted
           in map sumBoth grouped
  where
    sumBoth [(a, b, d), (_, _, d')] = (a, b, d + d')
    sumBoth _ = error "sumBoth: unexpected grouping"

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let rawTriples = map parseLine $ lines content
      triples = sumTriples rawTriples
      part1 = findMaxCircuit triples True
      part2 = findMaxCircuit triples False
  print part1
  print part2
