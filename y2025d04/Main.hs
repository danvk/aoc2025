-- https://adventofcode.com/2025/day/4

import Data.Map qualified as M
import Grid
import System.Environment (getArgs)

charAtPoint :: Grid -> Point -> Char
charAtPoint g pt = M.findWithDefault '.' pt g

numNeighbors :: Size -> Grid -> Point -> Int
numNeighbors dims g pt = length $ filter (\n -> charAtPoint g n == '@') (neighbors dims pt)

step :: Size -> Grid -> Grid
step dims@(w, h) g =
  M.fromList
    [ ((x, y), if isInaccessibleRoll (x, y) then '@' else '.')
      | x <- [0 .. w - 1],
        y <- [0 .. h - 1]
    ]
  where
    isInaccessibleRoll pt = charAtPoint g pt == '@' && numNeighbors dims g pt >= 4

numRolls :: Grid -> Int
numRolls g = length $ filter id $ map (== '@') $ M.elems g

-- Iterate until another function stops changing
iterateUntilStall :: (Eq b) => (a -> a) -> (a -> b) -> a -> (Int, b, a)
iterateUntilStall stepFn stallFn initState = (n, stallVal, state)
  where
    states = iterate stepFn initState
    tuples = zip (map stallFn states) $ zip [0 ..] states
    (stallVal, (n, state)) = firstStall tuples
    firstStall (hd@(s1, _) : tl@((s2, _) : _)) = if s1 == s2 then hd else firstStall tl
    firstStall _ = error "no stall"

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let (dims, initState) = parseGrid content
      initCount = numRolls initState
      part1 = initCount - numRolls (step dims initState)
      (n, finalCount, _) = iterateUntilStall (step dims) numRolls initState
      part2 = initCount - finalCount
  putStrLn $ "Part 1: " ++ show part1
  putStrLn $ "Part 2: " ++ show part2 ++ "; Stalled after " ++ show n ++ " steps with " ++ show finalCount ++ " rolls."
