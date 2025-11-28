-- https://adventofcode.com/2015/day/6
import System.Environment (getArgs)
import Data.List.Split
import qualified Data.Map.Strict as Map

-- toPair :: Show a => [a] -> (a, a)
-- toPair [a, b] = (a, b)
-- toPair x = error $ "Expected two elements, got " ++ show x

-- (|>) :: (a->b) -> (b->c) -> (a->c)
-- (|>) f g x = g (f x)

data Point = Point Int Int deriving (Show)

parseXY :: String -> Point
parseXY xy = case map read (splitOn "," xy) of
    [x, y] -> Point x y
    _ -> error $ "Invalid point: " ++ xy

-- turn on 0,0 through 999,999
-- toggle 0,0 through 999,0
-- turn off 499,499 through 500,500

data Command = On | Off | Toggle deriving (Show)
data Instruction = Instruction Command Point Point deriving (Show)

parseLine :: String -> Instruction
parseLine str = case words str of
    ["turn", "on", a, "through", b] -> Instruction On (parseXY a) (parseXY b)
    ["toggle", a, "through", b] -> Instruction Toggle (parseXY a) (parseXY b)
    ["turn", "off", a, "through", b] -> Instruction Off (parseXY a) (parseXY b)
    _ -> error $ "Invalid line: " ++ str

mergeLight :: Command -> Bool -> Bool
mergeLight On _ = True
mergeLight Off _ = False
mergeLight Toggle x = not x

updateMap :: Ord a => Map.Map a b -> Map.Map a b -> Map.Map a b
updateMap m1 m2 = Map.union m2 m1

applyOp :: Map.Map (Int, Int) Bool -> Instruction -> Map.Map (Int, Int) Bool
applyOp  m (Instruction op (Point x1 y1) (Point x2 y2)) =
    let rect = [(x, y) | x <- [x1..x2], y <- [y1..y2]] in
    let updates = Map.fromList $ map (\xy -> (xy, mergeLight op (Map.findWithDefault False xy m))) rect
    in updateMap m updates

countTrues :: Ord a => Map.Map a Bool -> Int
countTrues m = length [pos | (pos, v) <- Map.toList m, v]

mergeLight2 :: Command -> Int -> Int
mergeLight2 On x = x + 1
mergeLight2 Off x = max 0 (x - 1)
mergeLight2 Toggle x = x + 2

-- TOOD: combine with applyOp
applyOp2 :: Map.Map (Int, Int) Int -> Instruction -> Map.Map (Int, Int) Int
applyOp2 m (Instruction op (Point x1 y1) (Point x2 y2)) =
    let rect = [(x, y) | x <- [x1..x2], y <- [y1..y2]] in
    let updates = Map.fromList $ map (\xy -> (xy, mergeLight2 op (Map.findWithDefault 0 xy m))) rect
    in updateMap m updates

main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args
    content <- readFile inputFile
    let commands = map parseLine $ lines content
    -- print commands
    let grid = foldl applyOp Map.empty commands
    -- print grid
    let part1 = countTrues grid
    print part1

    let grid2 = foldl applyOp2 Map.empty commands
    let part2 = sum (map snd (Map.toList grid2))
    print part2


-- (a->b) -> (b->c) -> (a->c)