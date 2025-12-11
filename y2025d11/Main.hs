-- https://adventofcode.com/2025/day/11

import AocLib
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import System.Environment (getArgs)

toposort :: (Ord a) => (a -> [a]) -> a -> [a]
toposort neighborFn start = fst $ dfs start S.empty []
  where
    dfs node visited stack
      | node `S.member` visited = (stack, visited)
      | otherwise =
          let newVis = S.insert node visited
              (newStack, finalVis) = foldl processNeighbor (stack, newVis) (neighborFn node)
           in (node : newStack, finalVis)
    processNeighbor (st, vis) neighbor = dfs neighbor vis st

parseLine :: String -> (String, [String])
parseLine str = case words (eraseChars ":" str) of
  input : outputs -> (input, outputs)
  _ -> error $ "Unable to parse " ++ str

getCounts :: M.Map String [String] -> [String] -> String -> M.Map String Int
getCounts g nodes start = go nodes (M.fromList [(start, 1)])
  where
    go [] m = m
    go (n : ns) m = go ns (M.unionWith (+) m nextCounts)
      where
        meCount = M.findWithDefault 0 n m
        outs = M.findWithDefault [] n g
        nextCounts = M.fromList $ map (,meCount) outs

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let edges = map parseLine $ lines content
      sortedNodes = toposort (\n -> M.findWithDefault [] n g) "svr"
      g = M.fromList edges
      part1 = getCounts g sortedNodes "you" M.! "out"
  print part1
  -- print sortedNodes

  let svrCounts = getCounts g sortedNodes "svr"
      fftCounts = getCounts g sortedNodes "fft"
      dacCounts = getCounts g sortedNodes "dac"
      svrFft = M.findWithDefault 0 "fft" svrCounts
      svrDac = M.findWithDefault 0 "dac" svrCounts
      fftDac = M.findWithDefault 0 "dac" fftCounts
      dacFft = M.findWithDefault 0 "fft" dacCounts
      dacOut = M.findWithDefault 0 "out" dacCounts
      fftOut = M.findWithDefault 0 "out" fftCounts
      part2 = svrFft * fftDac * dacOut + svrDac * dacFft * fftOut
  print part2
