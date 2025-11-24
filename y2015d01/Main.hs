import Data.List (sort, findIndex)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)

readMove :: Char -> Int
readMove '(' = 1
readMove ')' = -1

rollingSumHelp :: Num a => a -> [a] -> [a]
rollingSumHelp _ [] = []
rollingSumHelp tally (x:xs) = (tally+x):rollingSumHelp (x+tally) xs

rollingSum :: Num a => [a] -> [a]
rollingSum = rollingSumHelp 0

main = do
    args <- getArgs
    let inputFile = head args
    content <- readFile inputFile
    let numbers = map readMove content
    let answer1 = sum numbers
    print answer1
    -- print numbers
    let positions = rollingSum numbers
    -- print positions
    let answer2 = (+ 1) <$> findIndex (< 0) positions
    print answer2
