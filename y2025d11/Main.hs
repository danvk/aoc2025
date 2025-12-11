-- https://adventofcode.com/2025/day/11

import AocLib
import Data.Heap qualified
import Data.Map.Strict qualified as M
import System.Environment (getArgs)

parseLine :: String -> (String, [String])
parseLine str = case words (eraseChars ":" str) of
  input : outputs -> (input, outputs)
  _ -> error $ "Unable to parse " ++ str

bfs :: (a -> [a]) -> (a -> Int) -> (a -> Bool) -> [a] -> [a]
bfs stepFn weight done starts = go initHeap
  where
    initList = zip (map weight starts) starts
    initHeap = Data.Heap.fromList initList `asTypeOf` (undefined :: Data.Heap.MinPrioHeap Int a)
    go h = case Data.Heap.view h of
      Just ((_, val), rest) ->
        let next = go (insertAll rest $ map (\x -> (weight x, x)) (stepFn val))
         in if done val then val : next else next
      Nothing -> []
    insertAll = foldr Data.Heap.insert

step :: M.Map String [String] -> [String] -> [[String]]
step m path = map (: path) $ M.findWithDefault [] (head path) m

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let g = M.fromList $ map parseLine $ lines content
      solns = bfs (step g) length (\x -> head x == "out") [["you"]]
  print $ length solns
