-- https://adventofcode.com/2015/day/5
import System.Environment (getArgs)
import Data.List
import Data.Maybe

isLongerThan :: Int -> [a] -> Bool
isLongerThan n list = not (null (drop (n-1) list))

isVowel :: Char -> Bool
isVowel x = x `elem` "aeiou"

hasThreeVowels :: String -> Bool
hasThreeVowels str = isLongerThan 3 $ filter isVowel str

hasTwoInRow :: String -> Bool
hasTwoInRow str = let pairs = zip str (tail str)
    in isJust $ find (uncurry (==)) pairs

hasSelectedSubstr :: String -> Bool
hasSelectedSubstr str = any (`isInfixOf` str) ["ab", "cd", "pq", "xy"]

isNice :: String -> Bool
isNice s = hasThreeVowels s && hasTwoInRow s && not (hasSelectedSubstr s)

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
