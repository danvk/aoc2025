-- https://adventofcode.com/2015/day/9
import System.Environment (getArgs)
import Data.Maybe
import Data.List
import Data.Function

type Trip = (String, String, Int)

-- London to Dublin = 464
parseLine :: String -> Trip
parseLine x = case words x of
    [a, "to", b, "=", d] -> (a, b, read d)
    _ -> error $ "unabled to parse " ++ x

removeCity :: [Trip] -> String -> [Trip]
removeCity trips city = filter (not . matchesCity) trips
    where matchesCity (a, b, _) = a == city || b == city

-- Current location -> remaining trips -> possible next stops
nextStops :: String -> [Trip] -> [(Int, String)]
nextStops city = mapMaybe nextStop
    where nextStop (a, b, d) = if city == a then Just (d, b) else Nothing

-- (distance, cities (head is current), possible remaining trips)
stepOne :: (Int, [String], [Trip]) -> [(Int, [String], [Trip])]
stepOne (d, path@(city:_), trips) = map (\(d', stop) -> (d+d', stop:path, removeCity trips city)) (nextStops city trips)
stepOne (_, _, []) = []  -- we're stuck!
stepOne (_, [], _) = error "empty path"

step :: [(Int, [String], [Trip])] -> [(Int, [String], [Trip])]
step trips = trips >>= stepOne

main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args
    content <- readFile inputFile
    let oneTrips = map parseLine $ lines content
        trips = oneTrips ++ map (\(a, b, d) -> (b, a, d)) oneTrips
        cities = nub $ map (\(a,_,_) -> a) trips
        starts = map (\c -> (0, [c], trips)) cities
    print trips
    print cities
    -- print $ removeCity trips "London"
    -- print $ nextStops "Dublin" trips
    let circuits = iterate step starts !! (length cities - 1)
        minCircuit = minimumBy (compare `on` (\(d, _, _) -> d)) circuits
        maxCircuit = maximumBy (compare `on` (\(d, _, _) -> d)) circuits
    print minCircuit
    print maxCircuit
