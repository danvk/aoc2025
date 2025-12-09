-- https://adventofcode.com/2016/day/19

import Data.List
import Data.Map.Strict qualified as M
import Debug.Trace
import System.Environment (getArgs)

-- whose turn, elf -> next elf, elf -> num presents
type State = (Int, M.Map Int Int, M.Map Int Int)

-- attacker, prev(target), elf -> next elf, elf -> num presents
type State2 = (Int, Int, M.Map Int Int, M.Map Int Int)

step :: State -> State
step (idx, nexts, presents) = (nextNext, nextNexts, nextPresents)
  where
    next = nexts M.! idx
    nextNext = nexts M.! next
    nextNexts = M.insert idx nextNext (M.delete next nexts)
    numStolen = presents M.! next
    nextPresents = M.adjust (numStolen +) idx (M.delete next presents)

step2 :: State2 -> State2
step2 (attacker, preTarget, nexts, presents) =
  (nextAttacker, nextPreTarget, nextNexts, nextPresents)
  where
    numElves = length nexts
    target = nexts M.! preTarget
    postTarget = nexts M.! target
    nextNexts = M.insert preTarget postTarget (M.delete target nexts)
    numStolen = presents M.! target
    nextPresents = M.adjust (numStolen +) attacker (M.delete target presents)
    nextAttacker = nextNexts M.! attacker
    preTarget1 = nextNexts M.! preTarget
    nextPreTarget = if even numElves then preTarget else preTarget1

main :: IO ()
main = do
  args <- getArgs
  let numElves = (read @Int) (head args)
      nextElf = M.fromList $ zip (numElves : [1 .. (numElves - 1)]) [1 .. numElves]
      presents = M.fromList $ map (,1 :: Int) [1 .. numElves]
      state = (1, numElves `div` 2, nextElf, presents)
      states = iterate step2 state
      part1 = find (\(_, _, n, _) -> length n == 1) states
  print part1
