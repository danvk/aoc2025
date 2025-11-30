-- https://adventofcode.com/2015/day/8
import System.Environment (getArgs)
import Data.Char

parse :: String -> String
parse "" = ""
parse ('\\':'\\':xs) = '\\':parse xs
parse ('\\':'"':xs) = '"':parse xs
-- parse ('\\':'x':a:b:xs) = show (read "0x" ++ [a, b]) ++ parse xs
parse ('\\':'x':a:b:xs) = chr (read ("0x" ++ [a, b]) :: Int) : parse xs
parse ('"':xs) = parse xs
parse (x:xs) = x:parse xs

main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args
    content <- readFile inputFile
    let raws = lines content
        parsed = map parse raws
    -- print raws
    -- print parsed
    let diffs = zipWith (\ a b -> length a - length b) raws parsed
        part1 = sum diffs
    print part1
