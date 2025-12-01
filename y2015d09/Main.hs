-- https://adventofcode.com/2015/day/9

import AocLib
import Circuit
import System.Environment (getArgs)

type Trip = (String, String, Int)

-- London to Dublin = 464
parseLine :: String -> Trip
parseLine x = case words x of
  [a, "to", b, "=", d] -> (a, b, read d)
  _ -> error $ "unabled to parse " ++ x

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let oneTrips = map parseLine $ lines content
      trips = oneTrips ++ map (\(a, b, d) -> (b, a, d)) oneTrips
      circuits = findCircuits trips False
      minCircuit = minUsing fst circuits
      maxCircuit = maxUsing fst circuits
  print minCircuit
  print maxCircuit
