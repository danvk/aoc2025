module Circuit (findCircuits, findMinCircuit, findMaxCircuit, Triple) where

import AocLib
import Data.List
import Data.Map qualified as M
import Data.Maybe

type Triple = (String, String, Int)

first2 :: (a, b, c) -> (a, b)
first2 (a, b, _) = (a, b)

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

-- Add the score for the last pair to complete the circle
completeCircuit :: M.Map (String, String) Int -> (Int, [String]) -> (Int, [String])
completeCircuit ds (d, path) = (d + d', path)
  where
    d' = ds M.! (head path, last path)

findCircuits :: [Triple] -> Bool -> [(Int, [String])]
findCircuits pairs isClosed = if isClosed then map (completeCircuit costMap) openCircuits else openCircuits
  where
    costMap = M.fromList [((a, b), d) | (a, b, d) <- pairs]
    people = nub $ map (\(x, _, _) -> x) pairs
    person1 = head people
    -- all starts are equivalent in a closed loop, but not in an open one
    starts = if isClosed then [(0, [person1], pairs)] else map (\c -> (0, [c], pairs)) people
    steps = iterate step starts !! (length people - 1)
    openCircuits = map first2 steps

-- The Bool = does the circuit need to be closed?
findMaxCircuit :: [Triple] -> Bool -> (Int, [String])
findMaxCircuit = (maxUsing fst .) . findCircuits

findMinCircuit :: [Triple] -> Bool -> (Int, [String])
findMinCircuit = (minUsing fst .) . findCircuits
-- ^ why the extra "." after fst?
