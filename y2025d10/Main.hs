-- https://adventofcode.com/2025/day/10

import AocLib
import Data.Bits
import Data.List.Split
import Data.Map.Strict qualified as M
import System.Environment (getArgs)

data Machine = Machine
  { target :: Int,
    targetStr :: String,
    buttons :: [Int],
    buttonsMap :: [M.Map Int Int],
    joltages :: [Int]
  }
  deriving (Show, Eq)

parseDiagram :: String -> Int
parseDiagram [] = 0
parseDiagram ('.' : xs) = 2 * parseDiagram xs
parseDiagram ('#' : xs) = 1 + 2 * parseDiagram xs
parseDiagram s = error $ "Invalid wiring diagram: " ++ s

parseButton :: String -> [Int]
parseButton s = map loudRead (splitOn "," s)

parseLine :: String -> Machine
parseLine str = case words (eraseChars "[](){}" str) of
  (target : rest) ->
    let buttons = map parseButton $ init rest
     in Machine
          { target = parseDiagram target,
            targetStr = target,
            buttons = map (foldr (\b acc -> acc + 2 ^ b) 0) buttons,
            buttonsMap = map (M.fromList . map (,1)) buttons,
            joltages = map (loudRead @Int) $ splitOn "," $ last rest
          }
  _ -> error $ "Unable to parse " ++ str

-- pressButton :: Int -> Int -> Int
-- pressButton a b = a `xor` b

-- bit mask of unknown variables, list of joltage = bitvector
machineToEq :: Machine -> (Int, [(Int, Int)])
machineToEq m = (2 ^ length (buttons m) - 1, eqs)
  where
    eqs = [(bitmaskForIndex i, target) | (i, target) <- zip [0 ..] (joltages m)]
    btns = buttons m
    pairs = zip [0 :: Int ..] btns
    bitmaskForIndex i = sum $ [if b .&. (1 `shiftL` i) > 0 then 2 ^ j else 0 | (j, b) <- pairs]

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let machines = map parseLine $ lines content
  -- print $ map (\m -> (length $ targetStr m, length $ buttons m)) machines
  print machines
  print $ map machineToEq machines

-- part1 = sum $ map ((fst . fromJust) . solveMachine) machines
-- print part1
-- print machines
-- let solns = zip [0 :: Int ..] $ map (fromJust . solveMachine2) machines
-- mapM_ (\x -> print x >> hFlush stdout) solns
-- let part2 = sum $ map (fst . snd) solns
-- print part2
