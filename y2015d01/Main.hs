import Data.List (sort)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)

readMove :: Char -> Int
readMove '(' = 1
readMove ')' = -1


main = do
    args <- getArgs
    let inputFile = head args
    content <- readFile inputFile
    let numbers = map readMove content
    let answer1 = sum numbers
    print answer1

