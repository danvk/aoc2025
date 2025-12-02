-- https://adventofcode.com/2015/day/14
import System.Environment (getArgs)

-- This introduces name, speed, flySecs, restSecs functions
data Reindeer = Reindeer
  { name :: String,
    speed :: Int,
    flySecs :: Int,
    restSecs :: Int
  }
  deriving (Show)

-- Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.
parseLine :: String -> Reindeer
parseLine str = case words str of
  [name, "can", "fly", speed, "km/s", "for", flySecs, "seconds,", "but", "then", "must", "rest", "for", restSecs, "seconds."] ->
    Reindeer {name = name, speed = read speed, flySecs = read flySecs, restSecs = read restSecs}
  _ -> error $ "Unable to parse " ++ str

locationAt :: Int -> Reindeer -> Int
locationAt t r = v * flySecs r * numIntervals + v * thisInterval
  where
    v = speed r
    interval = flySecs r + restSecs r
    numIntervals = t `div` interval
    thisInterval = min (flySecs r) (t `mod` interval)

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let reindeer = map parseLine $ lines content
      names = map name reindeer
      part1 = maximum (map (locationAt 2503) reindeer)
  print reindeer
  print $ zip names (map (locationAt 1000) reindeer)
  print $ zip names (map (locationAt 2503) reindeer)
  print part1
