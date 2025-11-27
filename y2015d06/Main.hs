-- https://adventofcode.com/2015/day/6
import System.Environment (getArgs)

import Data.List.Split

toPair :: Show a => [a] -> (a, a)
toPair [a, b] = (a, b)
toPair x = error $ "Expected two elements, got " ++ show x

-- (|>) :: (a->b) -> (b->c) -> (a->c)
-- (|>) f g x = g (f x)

parseXY :: String -> (Int, Int)
parseXY xy = toPair $ map read (splitOn "," xy)

-- turn on 0,0 through 999,999
-- toggle 0,0 through 999,0
-- turn off 499,499 through 500,500

-- TODO: use a data type
parseLine :: String -> (String, (Int, Int), (Int, Int))
parseLine str = case words str of
    ["turn", "on", a, "through", b] -> ("on", parseXY a, parseXY b)
    ["toggle", a, "through", b] -> ("toggle", parseXY a, parseXY b)
    ["turn", "off", a, "through", b] -> ("off", parseXY a, parseXY b)
    _ -> error $ "Invalid line: " ++ str

main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args
    content <- readFile inputFile
    let commands = map parseLine $ lines content
    print commands

-- (a->b) -> (b->c) -> (a->c)