-- https://adventofcode.com/2015/day/5
import System.Environment (getArgs)
import Data.List

isLongerThan :: Int -> [a] -> Bool
isLongerThan n list = not (null (drop (n-1) list))

hasThreeVowels :: String -> Bool
hasThreeVowels str = isLongerThan 3 $ filter (`elem` "aeiou") str

hasTwoInRow :: String -> Bool
hasTwoInRow str = let pairs = zip str (tail str)
    in any (uncurry (==)) pairs

hasSelectedSubstr :: String -> Bool
hasSelectedSubstr str = any (`isInfixOf` str) ["ab", "cd", "pq", "xy"]

isNice :: String -> Bool
isNice = allOf [hasThreeVowels, hasTwoInRow, not . hasSelectedSubstr]

hasDoublePair :: String -> Bool
hasDoublePair (a:b:xs) = isInfixOf [a, b] xs || hasDoublePair (b:xs)
hasDoublePair _ = False

hasSandwich :: String -> Bool
hasSandwich (a:b:c:xs) = a == c || hasSandwich (b:c:xs)
hasSandwich _ = False

isNice2 :: String -> Bool
isNice2 = allOf [hasDoublePair, hasSandwich]

allOf :: [a -> Bool] -> a -> Bool
allOf fns x = all (\fn -> fn x) fns

main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args
    content <- readFile inputFile
    let strs = lines content
    -- let nices = zip strs (map isNice strs)
    -- print nices
    let part1 = length $ filter isNice strs
    print part1
    let part2 = length $ filter isNice2 strs
    print part2
