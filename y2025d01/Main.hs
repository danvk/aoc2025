-- https://adventofcode.com/2025/day/1
import System.Environment (getArgs)

-- L30
-- R48

parseLine :: String -> Int
parseLine ('L':xs) = -(read xs)
parseLine ('R':xs) = read xs
parseLine line = error $ "Invalid line " ++ line

turn :: Int -> Int -> Int
turn a b = (a + b) `mod` 100

main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args
    content <- readFile inputFile
    let rotations = map parseLine $ lines content
        positions = scanl turn 50 rotations
        zeros = filter (== 0) positions
        part1 = length zeros
    print part1
