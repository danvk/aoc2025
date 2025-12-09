-- https://adventofcode.com/2016/day/19

import Data.List
import Data.Map.Strict qualified as M
import Debug.Trace
import System.Environment (getArgs)

-- whose turn, elf -> next elf, elf -> num presents
type State = (Int, M.Map Int Int, M.Map Int Int)

step :: State -> State
step (idx, nexts, presents) = (nextNext, nextNexts, nextPresents)
  where
    next = nexts M.! idx
    nextNext = nexts M.! next
    nextNexts = M.insert idx nextNext (M.delete next nexts)
    numStolen = presents M.! next
    nextPresents = M.adjust (numStolen +) idx (M.delete next presents)

step2 :: State -> State
step2 (idx, nexts, presents) = trace ("target: " ++ show next) (nextElf, nextNexts, nextPresents)
  where
    numElves = length nexts
    numAdvance = numElves `div` 2
    chain = iterate (nexts M.!) idx
    seq3 = drop (numAdvance - 1) chain
    [preNext, next, nextNext] = take 3 seq3
    nextNexts = M.insert preNext nextNext (M.delete next nexts)
    numStolen = presents M.! next
    nextPresents = M.adjust (numStolen +) idx (M.delete next presents)
    nextElf = nextNexts M.! idx

main :: IO ()
main = do
  args <- getArgs
  let numElves = (read @Int) (head args)
      nextElf = M.fromList $ zip (numElves : [1 .. (numElves - 1)]) [1 .. numElves]
      presents = M.fromList $ map (,1 :: Int) [1 .. numElves]
      state = (1, nextElf, presents)
      states = iterate step2 state
      part1 = find (\(_, n, _) -> length n == 1) states
  print part1
