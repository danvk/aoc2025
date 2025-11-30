-- https://adventofcode.com/2015/day/10
import System.Environment (getArgs)
import Data.List

lookAndSay :: [Int] -> [Int]
lookAndSay xs = group xs >>= \x -> [length x, head x]

main :: IO ()
main = do
    args <- getArgs
    -- XXX what's an idiomatic way to do this without a non-exhaustive warning?
    let [startStr, timesStr] = args
        start = map (\c -> read [c] :: Int) startStr
        times = read timesStr :: Int
    print start
    print times
    let out = iterate lookAndSay start !! times
    -- print out
    print $ length out
