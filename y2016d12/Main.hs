-- https://adventofcode.com/2016/day/12

import AocLib
import Data.List
import Data.Map.Strict qualified as M
import System.Environment (getArgs)
import Text.Read (readMaybe)

-- cpy x y copies x (either an integer or the value of a register) into register y.
-- inc x increases the value of register x by one.
-- dec x decreases the value of register x by one.
-- jnz x y jumps to an instruction y away (positive means forward; negative means backward), but only if x is not zero.

data Instr
  = CpyReg Char Char
  | CpyInt Int Char
  | Inc Char
  | Dec Char
  | JnzReg Char Int
  | JnzInt Int Int
  deriving (Eq, Show)

parseLine :: String -> Instr
parseLine str = case words (eraseChars "+," str) of
  ["inc", [r]] -> Inc r
  ["dec", [r]] -> Dec r
  ["cpy", src, [r]] -> case readMaybe src of
    Just n -> CpyInt n r
    Nothing -> CpyReg (head src) r
  ["jnz", src, offset] -> case readMaybe src of
    Just n -> JnzInt n (read offset)
    Nothing -> JnzReg (head src) (read offset)
  _ -> error $ "Unable to parse " ++ str

type State = (Int, M.Map Char Int)

exec :: Instr -> State -> State
exec (Inc r) (n, regs) = (n + 1, M.adjust (+ 1) r regs)
exec (Dec r) (n, regs) = (n + 1, M.adjust (+ (-1)) r regs)
exec (CpyInt num r) (n, regs) = (n + 1, M.insert r num regs)
exec (CpyReg src r) (n, regs) = (n + 1, M.insert r (regs `safeLookup` src) regs)
exec (JnzReg r offset) (n, regs) = (if (regs `safeLookup` r) /= 0 then n + offset else n + 1, regs)
exec (JnzInt num offset) (n, regs) = (if num /= 0 then n + offset else n + 1, regs)

(!?) :: (Ord t, Num t) => [a] -> t -> Maybe a
[] !? _ = Nothing
(x : xs) !? i
  | i < 0 = Nothing
  | i == 0 = Just x
  | otherwise = xs !? (i - 1)

safeLookup :: (Show a, Ord a) => M.Map a b -> a -> b
safeLookup m x = case M.lookup x m of
  Just y -> y
  Nothing -> error $ "Failed to look up " ++ show x

-- TODO: is there a vector type for fast indexing?
step :: [Instr] -> State -> Maybe State
step instrs s@(idx, _) = fmap (`exec` s) (instrs !? idx)

iterateToNone :: (a -> Maybe a) -> a -> [a]
iterateToNone f = unfoldr (\x -> (x,) <$> f x)

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let instrs = map parseLine $ lines content
      initState = (0, M.fromList [('a', 0), ('b', 0), ('c', 0), ('d', 0)])
      part1 = last $ iterateToNone (step instrs) initState
      initState2 = (0, M.fromList [('a', 0), ('b', 0), ('c', 1), ('d', 0)])
      part2 = last $ iterateToNone (step instrs) initState2
  print part1
  print part2
