-- https://adventofcode.com/2025/day/11

import AocLib
import Data.List
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import System.Environment (getArgs)

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

toposort :: (Ord a) => (a -> [a]) -> [a] -> [a]
toposort neighborFn nodes = fst $ foldr foldFn ([], S.empty) nodes
  where
    dfs [] visited = ([], visited)
    dfs (x : xs) visited =
      if x `S.member` visited
        then
          dfs xs visited
        else
          let (nstack, nvisited) = dfs (neighborFn x ++ xs) (S.insert x visited)
           in (x : nstack, nvisited)
    foldFn n (stack, visited) = let (nstack, nvisited) = dfs [n] visited in (nstack ++ stack, nvisited)

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
      sortedNodesFile = args !! 1
  content <- readFile inputFile
  content2 <- readFile sortedNodesFile
  let edges = map parseLine $ lines content
      sortedNodes = lines content2
      -- nodes = nub $ concatMap (uncurry (:)) edges
      g = M.fromList edges
      part1 = getCounts g sortedNodes "you" M.! "out"
  print part1

-- putStrLn $ intercalate "\n" $ toposort (\n -> M.findWithDefault [] n g) nodes

-- descendents = M.fromList $ map (\k -> (k, floodFill (\n -> M.findWithDefault [] n g) [k])) nodes
-- sortedNodes = sortBy (nodeOrder descendents) nodes

-- putStrLn $ intercalate "\n" sortedNodes

-- print sortedNodes

-- part1 = getCounts edges "you" M.! "out"

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
