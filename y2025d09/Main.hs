-- https://adventofcode.com/2025/day/9

import AocLib
import Data.List
import Data.List.Split
import Data.Map.Strict qualified as M
import Data.Ord qualified
import Grid
import System.Environment (getArgs)

parseLine :: String -> Point
parseLine str = case splitOn "," str of
  [a, b] -> (read a, read b)
  _ -> error $ "Unable to parse " ++ str

area :: Point -> Point -> Int
area (x1, y1) (x2, y2) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

strokePath :: Grid -> [Point] -> Grid
strokePath initG pts = g'
  where
    pairs = zip pts $ tail pts ++ [head pts]
    stroke ((x1, y1), (x2, y2)) g
      | x1 == x2 = M.union g (M.fromList [((x1, y), 'X') | y <- [min y1 y2 + 1 .. max y1 y2 - 1]])
      | y1 == y2 = M.union g (M.fromList [((x, y1), 'X') | x <- [min x1 x2 + 1 .. max x1 x2 - 1]])
      | otherwise = error "Invalid pair"
    g' = foldr stroke initG pairs

strokePathForTesting :: [Point] -> [Point]
strokePathForTesting pts = g'
  where
    pairs = zip pts $ tail pts ++ [head pts]
    stroke ((x1, y1), (x2, y2))
      | x1 == x2 = []
      | y1 == y2 = [(x, y1) | x <- [min x1 x2 .. max x1 x2 - 1]]
      | otherwise = error "Invalid pair"
    g' = concatMap stroke pairs

isInteriorPt :: M.Map Int [Int] -> Point -> Bool
isInteriorPt testPtsByX (x, y) = odd $ length $ filter (< y) $ M.findWithDefault [] x testPtsByX

perimeter :: Point -> Point -> [Point]
perimeter (x1, y1) (x2, y2) =
  [(x, y) | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2, max y1 y2]]
    ++ [(x, y) | x <- [min x1 x2, max x1 x2], y <- [min y1 y2 .. max y1 y2]]

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let pts = map parseLine $ lines content
      part1 = maximum $ [area p1 p2 | p1 <- pts, p2 <- pts, p1 < p2]
      g = M.fromList [(pt, '#') | pt <- pts]
      testPts = strokePathForTesting pts
      testPtsByX = M.fromList $ map (\(x, xys) -> (x, nub $ sort $ map snd xys)) $ groupByFn fst testPts
      g' = strokePath g pts
      rects = sortOn (Data.Ord.Down . uncurry area) ([(p1, p2) | p1 <- pts, p2 <- pts, p1 < p2])
      part2 = filter (\(p1, p2) -> all (\p -> charAtPoint g' p /= '.' || isInteriorPt testPtsByX p) (perimeter p1 p2)) rects

  print part1
  print $ head $ zip (map (uncurry area) part2) part2
