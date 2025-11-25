-- https://adventofcode.com/2015/day/2
import System.Environment (getArgs)

splitOn char text = words [if c == char then ' ' else c | c <- text]

parseLine :: String -> (Int, Int, Int)
parseLine line =
    case parts of
        [a, b, c] -> (read a, read b, read c)
    where parts = splitOn 'x' line

sides :: (Int, Int, Int) -> [Int]
sides (l, w, h) = [l*w, w*h, h*l]

wrappingPaper :: (Int, Int, Int) -> Int
wrappingPaper dims = 2 * (sum (sides dims)) + (minimum (sides dims))

main = do
    args <- getArgs
    let inputFile = head args
    content <- readFile inputFile
    let presents = map parseLine $ lines content
    let squareFeet = map wrappingPaper presents
    let part1 = sum squareFeet
    print part1
