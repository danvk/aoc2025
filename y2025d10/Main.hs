-- https://adventofcode.com/2025/day/10

import AocLib
import Data.Bits
import Data.Heap qualified
import Data.List.Split
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import System.Environment (getArgs)
import System.IO

data Machine = Machine
  { target :: Int,
    targetStr :: String,
    buttons :: [Int],
    buttonsMap :: [M.Map Int Int],
    joltages :: M.Map Int Int
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
            joltages = M.fromList $ zip [0 ..] (map (loudRead @Int) $ splitOn "," $ last rest)
          }
  _ -> error $ "Unable to parse " ++ str

-- pressButton :: Int -> Int -> Int
-- pressButton a b = a `xor` b

bfs :: (Ord a) => (a -> [a]) -> (a -> Int) -> (a -> Bool) -> [a] -> Maybe a
bfs stepFn weight done starts = go initHeap S.empty
  where
    initList = zip (map weight starts) starts
    initHeap = Data.Heap.fromList initList `asTypeOf` (undefined :: Data.Heap.MinPrioHeap Int a)
    go h visited = case Data.Heap.view h of
      Just ((_, val), rest) ->
        if val `S.member` visited
          then go rest visited
          else
            if done val then Just val else go (insertAll rest $ map (\x -> (weight x, x)) (stepFn val)) (S.insert val visited)
      Nothing -> Nothing
    insertAll = foldr Data.Heap.insert

step :: Machine -> Int -> [Int]
step m x = map (xor x) (buttons m)

stepD :: (a -> [a]) -> ((Int, a) -> [(Int, a)])
stepD stepFn = fn
  where
    fn (n, pt) = map (n + 1,) $ stepFn pt

solveMachine :: Machine -> Maybe (Int, Int)
solveMachine m = bfs (stepD (step m)) fst (\(_, s) -> s == target m) [(0, 0)]

type State2 = M.Map Int Int

step2 :: Machine -> State2 -> [State2]
step2 m s = map (M.unionWith (+) s) (buttonsMap m)

solveMachine2 :: Machine -> Maybe (Int, State2)
solveMachine2 m = bfs (stepD (step2 m)) fst (\(_, s) -> s == joltages m) [(0, M.empty)]

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let machines = map parseLine $ lines content
  print $ map (\m -> (length $ targetStr m, length $ buttons m)) machines

-- part1 = sum $ map ((fst . fromJust) . solveMachine) machines
-- print part1
-- print machines
-- let solns = zip [0 :: Int ..] $ map (fromJust . solveMachine2) machines
-- mapM_ (\x -> print x >> hFlush stdout) solns
-- let part2 = sum $ map (fst . snd) solns
-- print part2
