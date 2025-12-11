-- https://adventofcode.com/2025/day/11

import AocLib
import Data.Heap qualified
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

getDistances :: (Ord a) => (a -> [a]) -> a -> [(Int, a)]
getDistances stepFn start = go initHeap S.empty
  where
    initList = [(0, start)]
    initHeap = Data.Heap.fromList initList `asTypeOf` (undefined :: Data.Heap.MinPrioHeap Int a)
    go h visited = case Data.Heap.view h of
      Just (n@(d, val), rest) ->
        if val `S.member` visited
          then go rest visited
          else
            n : go (insertAll rest $ map (1 + d,) (stepFn val)) (S.insert val visited)
      Nothing -> []
    insertAll = foldr Data.Heap.insert

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let edges = map parseLine $ lines content
      g = M.fromList edges
  print $ getDistances (\n -> M.findWithDefault [] n g) "svr"

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
