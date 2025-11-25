import Data.List (findIndex)
import System.Environment (getArgs)

readMove :: Char -> Int
readMove '(' = 1
readMove ')' = -1
readMove c = error $ "Invalid input character " ++ [c]

rollingSum :: [Int] -> [Int]
rollingSum = scanl1 (+)

main :: IO ()
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
