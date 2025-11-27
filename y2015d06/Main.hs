-- https://adventofcode.com/2015/day/6
import System.Environment (getArgs)
import Data.List.Split
import Data.Map.Strict as Map hiding (map, foldr)

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

applyOp :: (String, (Int, Int), (Int, Int)) -> Map (Int, Int) Bool -> Map (Int, Int) Bool
applyOp (op, (x1, y1), (x2, y2)) m =
    let rect = [(x, y) | x <- [x1..x2], y <- [y1..y2]] in
    let updates = Map.fromList $ map (\xy -> (xy, mergeLight op (Map.findWithDefault False xy m))) rect
    in updateMap m updates

countTrues :: Ord a => Map a Bool -> Int
countTrues m = length [pos | (pos, v) <- Map.toList m, v]

main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args
    content <- readFile inputFile
    let commands = map parseLine $ lines content
    let grid = foldr applyOp Map.empty commands
    let part1 = countTrues grid
    print part1


-- (a->b) -> (b->c) -> (a->c)