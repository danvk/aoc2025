-- https://adventofcode.com/2016/day/13
import Data.Bits
import Data.List
import System.Environment (getArgs)

isWall :: Int -> (Int, Int) -> Bool
isWall seed (x, y)
  | x < 0 || y < 0 = True
  | otherwise = odd $ popCount (seed + x * x + 3 * x + 2 * x * y + y + y * y)

main :: IO ()
main = do
  args <- getArgs
  let seed = (read @Int) $ head args
  putStrLn $ intercalate "\n" $ map (\y -> map (\x -> if isWall seed (x, y) then '#' else '.') [0 .. 9]) [0 .. 6]
