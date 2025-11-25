-- https://adventofcode.com/2015/day/2
import System.Environment (getArgs)

splitOn char text = words [if c == char then ' ' else c | c <- text]

parseLine :: String -> (Int, Int, Int)
parseLine line =
    case parts of
        [a, b, c] -> (read a, read b, read c)
    where parts = splitOn 'x' line

main = do
    args <- getArgs
    let inputFile = head args
    content <- readFile inputFile
    let theLines = map parseLine $ lines content
    print theLines
