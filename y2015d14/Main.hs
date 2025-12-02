-- https://adventofcode.com/2015/day/14
import System.Environment (getArgs)

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

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let x = map parseLine $ lines content
  print x
