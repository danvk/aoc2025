-- https://adventofcode.com/2015/day/13

import Data.Function
import Data.List
import Data.Map qualified as M
import Data.Maybe
import System.Environment (getArgs)

type Pair = (String, String, Int)

-- Carol would lose 62 happiness units by sitting next to Alice.
-- Carol would gain 60 happiness units by sitting next to Bob.
parseLine :: String -> Pair
parseLine line = case words line of
  [p1, "would", "lose", units, "happiness", "units", "by", "sitting", "next", "to", p2] -> (p1, filter (/= '.') p2, (-1) * read units)
  [p1, "would", "gain", units, "happiness", "units", "by", "sitting", "next", "to", p2] -> (p1, filter (/= '.') p2, read units)
  _ -> error $ "Invalid line: " ++ line

first2 :: (a, b, c) -> (a, b)
first2 (a, b, _) = (a, b)

-- Sum (A, B, d1) and (B, A, d2) -> [(A, B, d1+d2), (B, A, d1+d2)]
sumPairs :: [Pair] -> [Pair]
sumPairs pairs =
  let expanded = (pairs >>= (\p@(a, b, d) -> [p, (b, a, d)]))
   in let sorted = sort expanded
       in let grouped = groupBy (\a b -> first2 a == first2 b) sorted
           in map sumBoth grouped
  where
    sumBoth [(a, b, d), (_, _, d')] = (a, b, d + d')
    sumBoth _ = error ""

removeCity :: [Pair] -> String -> [Pair]
removeCity trips city = filter (not . matchesCity) trips
  where
    matchesCity (a, b, _) = a == city || b == city

-- Current location -> remaining trips -> possible next stops
nextStops :: String -> [Pair] -> [(Int, String)]
nextStops city = mapMaybe nextStop
  where
    nextStop (a, b, d) = if city == a then Just (d, b) else Nothing

-- (distance, cities (head is current), possible remaining trips)
stepOne :: (Int, [String], [Pair]) -> [(Int, [String], [Pair])]
stepOne (d, path@(city : _), trips) = map (\(d', stop) -> (d + d', stop : path, removeCity trips city)) (nextStops city trips)
stepOne (_, _, []) = [] -- we're stuck!
stepOne (_, [], _) = error "empty path"

step :: [(Int, [String], [Pair])] -> [(Int, [String], [Pair])]
step trips = trips >>= stepOne

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

minUsing :: (Ord b) => (a -> b) -> [a] -> a
minUsing fn = minimumBy (compare `on` fn)

maxUsing :: (Ord b) => (a -> b) -> [a] -> a
maxUsing fn = maximumBy (compare `on` fn)

-- Add the score for the last pair to complete the circle
completeCircuit :: M.Map (String, String) Int -> (Int, [String]) -> (Int, [String])
completeCircuit ds (d, path) = (d + d', path)
  where
    d' = ds M.! (head path, last path)

-- bool = does the circuit need to be closed?
findMinCircuit :: [Pair] -> Bool -> (Int, [String])
findMinCircuit rawPairs isClosed = maxUsing fst circuits
  where
    pairs = sumPairs rawPairs
    costMap = M.fromList [((a, b), d) | (a, b, d) <- pairs]
    people = nub (map fst3 pairs)
    person1 = head people
    -- all starts are equivalent in a closed loop, but not in an open one
    starts = if isClosed then [(0, [person1], pairs)] else map (\c -> (0, [c], pairs)) people
    steps = iterate step starts !! (length people - 1)
    openCircuits = map first2 steps
    circuits = if isClosed then map (completeCircuit costMap) openCircuits else openCircuits

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let rawPairs = map parseLine $ lines content
      part1 = findMinCircuit rawPairs True
      part2 = findMinCircuit rawPairs False
  print part1
  print part2
