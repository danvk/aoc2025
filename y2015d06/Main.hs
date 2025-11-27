-- https://adventofcode.com/2015/day/6
import System.Environment (getArgs)
import Data.List.Split
import Data.Map.Strict as Map hiding (map, foldl, foldr)

toPair :: Show a => [a] -> (a, a)
toPair [a, b] = (a, b)
toPair x = error $ "Expected two elements, got " ++ show x

-- (|>) :: (a->b) -> (b->c) -> (a->c)
-- (|>) f g x = g (f x)

parseXY :: String -> (Int, Int)
parseXY xy = toPair $ map read (splitOn "," xy)

-- turn on 0,0 through 999,999
-- toggle 0,0 through 999,0
-- turn off 499,499 through 500,500

-- TODO: use a data type
parseLine :: String -> (String, (Int, Int), (Int, Int))
parseLine str = case words str of
    ["turn", "on", a, "through", b] -> ("on", parseXY a, parseXY b)
    ["toggle", a, "through", b] -> ("toggle", parseXY a, parseXY b)
    ["turn", "off", a, "through", b] -> ("off", parseXY a, parseXY b)
    _ -> error $ "Invalid line: " ++ str

mergeLight :: String -> Bool -> Bool
mergeLight "on" _ = True
mergeLight "off" _ = False
mergeLight "toggle" x = not x
mergeLight op _ = error $ "Invalid op " ++ op

updateMap :: Ord a => Map a b -> Map a b -> Map a b
updateMap m1 m2 = Map.union m2 m1

applyOp :: Map (Int, Int) Bool -> (String, (Int, Int), (Int, Int)) -> Map (Int, Int) Bool
applyOp  m (op, (x1, y1), (x2, y2)) =
    let rect = [(x, y) | x <- [x1..x2], y <- [y1..y2]] in
    let updates = Map.fromList $ map (\xy -> (xy, mergeLight op (Map.findWithDefault False xy m))) rect
    in updateMap m updates

countTrues :: Ord a => Map a Bool -> Int
countTrues m = length [pos | (pos, v) <- Map.toList m, v]

mergeLight2 :: String -> Int -> Int
mergeLight2 "on" x = x + 1
mergeLight2 "off" x = max 0 (x - 1)
mergeLight2 "toggle" x = x + 2
mergeLight2 op _ = error $ "Invalid op " ++ op

-- TOOD: combine with applyOp
applyOp2 :: Map (Int, Int) Int -> (String, (Int, Int), (Int, Int)) -> Map (Int, Int) Int
applyOp2  m (op, (x1, y1), (x2, y2)) =
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