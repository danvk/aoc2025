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

clicks :: Int -> Int -> Int
clicks a b
    | a < b = length $ filter ((==) 0 . (`mod` 100)) [(a+1)..b]
    | otherwise = length $ filter ((==) 0 . (`mod` 100)) [(a-1),(a-2)..b]

-- rotations in [-998, 999]

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
    let abspos = scanl (+) 50 rotations
        -- part2 = sum $ zipWith clicks abspos (tail abspos)
        part2 = zip abspos (tail abspos)
        deb = map (uncurry clicks) part2
    -- print abspos
    print part2
    print deb
    print $ sum deb
