-- https://adventofcode.com/2015/day/10
import System.Environment (getArgs)

lookAndSay :: [Int] -> [Int]
lookAndSay [] = []
lookAndSay (x:xs) = (1 + length start):x:lookAndSay rest
    where (start, rest) = break (/= x) xs

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
