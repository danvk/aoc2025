-- https://adventofcode.com/2025/day/11

import AocLib
import Data.Heap qualified
import Data.List
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import System.Environment (getArgs)

parseLine :: String -> (String, [String])
parseLine str = case words (eraseChars ":" str) of
  input : outputs -> (input, outputs)
  _ -> error $ "Unable to parse " ++ str

getCounts :: [(String, [String])] -> String -> M.Map String Int
getCounts edges start = go edgesFromStart (M.fromList [(start, 1)])
  where
    edgesFromStart = dropWhile (\(n, _) -> n /= start) edges
    go [] m = m
    go ((n, outs) : xs) m = go xs (M.unionWith (+) m nextCounts)
      where
        meCount = M.findWithDefault 0 n m
        nextCounts = M.fromList $ map (,meCount) outs

floodFill :: (Ord a) => (a -> [a]) -> [a] -> [a]
floodFill neighborFn starts = tail $ go starts S.empty
  where
    go [] _ = []
    go (x : xs) visited =
      if x `S.member` visited
        then
          go xs visited
        else
          x : go (neighborFn x ++ xs) (S.insert x visited)

nodeOrder :: M.Map String [String] -> String -> String -> Ordering
nodeOrder descendents a b = case (b `elem` (descendents M.! a), a `elem` (descendents M.! b)) of
  (True, True) -> error $ "Graph has cycle " ++ a ++ " <-> " ++ b
  (True, False) -> LT
  (False, True) -> GT
  (False, False) -> EQ

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let edges = map parseLine $ lines content
      nodes = nub $ concatMap (uncurry (:)) edges
      g = M.fromList edges
      descendents = M.fromList $ map (\k -> (k, floodFill (\n -> M.findWithDefault [] n g) [k])) nodes
      sortedNodes = sortBy (nodeOrder descendents) nodes

  print sortedNodes

-- part1 = getCounts edges "you" M.! "out"
-- print part1
-- let svrCounts = getCounts edges "svr"
--     fftCounts = getCounts edges "fft"
--     dacCounts = getCounts edges "dac"
--     svrFft = M.findWithDefault 0 "fft" svrCounts
--     svrDac = M.findWithDefault 0 "dac" svrCounts
--     fftDac = M.findWithDefault 0 "dac" fftCounts
--     dacFft = M.findWithDefault 0 "fft" dacCounts
--     dacOut = M.findWithDefault 0 "out" dacCounts
--     fftOut = M.findWithDefault 0 "out" fftCounts
--     part2 = svrFft * fftDac * dacOut + svrDac * dacFft * fftOut
-- -- print svrCounts
-- print svrFft
-- print svrDac
-- print fftDac
-- print dacFft
-- print dacOut
-- print fftOut
-- print part2
