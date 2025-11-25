-- https://adventofcode.com/2015/day/3
import System.Environment (getArgs)
import qualified Data.Map.Strict as Map

toDir :: Char -> (Int, Int)
toDir '<' = (-1, 0)
toDir '>' = (1, 0)
toDir '^' = (0, -1)
toDir 'v' = (0, 1)
toDir c = error ("Invalid character '" ++ [c] ++ "'")

-- TODO: probably a better way to write this
move :: (Int, Int) -> (Int, Int) -> (Int, Int)
move (a, b) (c, d) = (a+c, b+d)

main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args
    content <- readFile inputFile
    let dirs = map toDir (head (lines content))
    let coords = scanl move (0, 0) dirs
    let counts = Map.fromListWith (+) (map (, 1::Int) coords)
    let part1 = Map.size counts
    print part1
