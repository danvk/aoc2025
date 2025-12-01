module Circuit (findMinCircuit, Triple) where

import Data.Function
import Data.List
import Data.Map qualified as M
import Data.Maybe

type Triple = (String, String, Int)

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

removeCity :: [Triple] -> String -> [Triple]
removeCity trips city = filter (not . matchesCity) trips
  where
    matchesCity (a, b, _) = a == city || b == city

-- Current location -> remaining trips -> possible next stops
nextStops :: String -> [Triple] -> [(Int, String)]
nextStops city = mapMaybe nextStop
  where
    nextStop (a, b, d) = if city == a then Just (d, b) else Nothing

-- (distance, cities (head is current), possible remaining trips)
stepOne :: (Int, [String], [Triple]) -> [(Int, [String], [Triple])]
stepOne (d, path@(city : _), trips) = map (\(d', stop) -> (d + d', stop : path, removeCity trips city)) (nextStops city trips)
stepOne (_, _, []) = [] -- we're stuck!
stepOne (_, [], _) = error "empty path"

step :: [(Int, [String], [Triple])] -> [(Int, [String], [Triple])]
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
findMinCircuit :: [Triple] -> Bool -> (Int, [String])
findMinCircuit rawTriples isClosed = maxUsing fst circuits
  where
    pairs = sumTriples rawTriples
    costMap = M.fromList [((a, b), d) | (a, b, d) <- pairs]
    people = nub (map fst3 pairs)
    person1 = head people
    -- all starts are equivalent in a closed loop, but not in an open one
    starts = if isClosed then [(0, [person1], pairs)] else map (\c -> (0, [c], pairs)) people
    steps = iterate step starts !! (length people - 1)
    openCircuits = map first2 steps
    circuits = if isClosed then map (completeCircuit costMap) openCircuits else openCircuits
