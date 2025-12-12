-- https://adventofcode.com/2016/day/24

import Data.Char (digitToInt)
import Data.Heap qualified
import Data.List
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Tuple
import GHC.Unicode
import Grid
import System.Environment (getArgs)

findNums :: Grid -> M.Map Int Point
findNums g = M.fromList [(digitToInt c, (x, y)) | ((x, y), c) <- M.toList g, isDigit c]

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

step :: Grid -> Point -> [Point]
step g pt = filter (\p -> charAtPoint g p /= '#') $ neighbors4 pt

pointPointDistance :: Grid -> M.Map Int Point -> M.Map (Int, Int) Int
pointPointDistance g numPts =
  M.fromList
    [ ((srcNum, dstNum), ds M.! dstPt)
      | (srcNum, ds) <- distances,
        (dstNum, dstPt) <- M.toList numPts
    ]
  where
    distances = [(num, M.fromList $ map swap $ getDistances (step g) pt) | (num, pt) <- M.toList numPts]

distanceForSeq :: M.Map (Int, Int) Int -> [Int] -> Int
distanceForSeq ds nums = sum $ zipWith (curry (ds M.!)) nums (tail nums)

part1 :: M.Map (Int, Int) Int -> Int -> Int
part1 ds numPts = minimum $ map (distanceForSeq ds . (0 :)) (permutations [1 .. (numPts - 1)])

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let (_, g) = parseGrid content
      numPts = findNums g
      distances = pointPointDistance g numPts
      p1 = part1 distances (length numPts)
  print p1

-- print $ distanceForSeq distances [0, 4, 1, 2, 3]
