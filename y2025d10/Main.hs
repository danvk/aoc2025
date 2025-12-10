-- https://adventofcode.com/2025/day/10

import AocLib
import Data.Bits
import Data.List.Split
import Data.Map.Strict qualified as M
import Debug.Trace
import System.Environment (getArgs)
import System.IO

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

-- all the ways that N numbers can sum to S
waysToSum :: Int -> Int -> [[Int]] -- S -> N -> list
waysToSum 0 0 = [[]]
waysToSum 0 n = [replicate n 0]
waysToSum _ 0 = []
waysToSum s n = concat [map (a :) $ waysToSum (s - a) (n - 1) | a <- [0 .. s]]

toBits :: Int -> [Int]
toBits 0 = []
toBits n = go 0 n
  where
    go _ 0 = []
    go i x = if x .&. (1 `shiftL` i) > 0 then i : go (i + 1) (x - (1 `shiftL` i)) else go (i + 1) x

substitute :: [(Int, Int)] -> (Int, Int) -> (Int, Int)
substitute [] vt = vt
substitute ((var, val) : rest) vt@(vars, target)
  | vars .&. (1 `shiftL` var) > 0 = substitute rest (vars - (1 `shiftL` var), target - val)
  | otherwise = substitute rest vt

isValidEq :: (Int, Int) -> Bool
isValidEq (0, 0) = True
isValidEq (0, _) = False
isValidEq (_, v) = v >= 0

-- Minimize the sum of the variables subject to the constraints.
minimizeSystem :: (Int, [(Int, Int)]) -> Int
minimizeSystem (0, _) = 0 -- nothing left to solve
minimizeSystem (remainingBits, eqs) =
  target
    + bestOfRest -- trace ("remainingBits: " ++ show remainingBits ++ " eqs=" ++ show eqs)
  where
    chosenEq@(eq, target) = minUsing (popCount . fst) eqs
    bits = toBits eq -- trace ("  chosenEq=" ++ show chosenEq)
    candidates = map (zip bits) $ waysToSum target (length bits)
    otherEqs = filter (chosenEq /=) eqs
    filteredCandidates =
      [ (remainingBits - eq, remainingEqs)
        | candidate <- candidates,
          remainingEqs <- [map (substitute candidate) otherEqs],
          all isValidEq remainingEqs
      ]
    bestOfRest =
      if null filteredCandidates
        then 100000 -- some big number
        else
          minimum $ map minimizeSystem filteredCandidates

-- need to filter out invalid solutions, with target < 0

augment :: [(Int, Int)] -> [(Int, Int)]
augment eqs = eqs ++ [(vb - va, tb - ta) | (va, ta) <- eqs, (vb, tb) <- eqs, tb > ta && va .&. vb == va]

solveMachine2 :: Machine -> Int
solveMachine2 m = minimizeSystem (rbs, augment eqs)
  where
    (rbs, eqs) = machineToEq m

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let machines = map parseLine $ lines content
  -- machine = head machines
  -- eqs = machineToEq machine
  -- print $ map (\m -> (length $ targetStr m, length $ buttons m)) machines
  -- print machine
  -- print eqs
  -- print $ minimizeSystem eqs

  -- part1 = sum $ map ((fst . fromJust) . solveMachine) machines
  -- print part1
  -- print machines
  let solns = zip [0 :: Int ..] $ map solveMachine2 machines
  mapM_ (\x -> print x >> hFlush stdout) solns
  print $ sum (map snd solns)

-- let part2 = sum $ map (fst . snd) solns
-- print part2
