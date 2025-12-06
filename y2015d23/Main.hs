-- https://adventofcode.com/2015/day/23

import Data.Map qualified as M
import System.Environment (getArgs)
import Text.Read (readMaybe)

data Instr
  = Hlf Char
  | Tpl Char
  | Inc Char
  | Jmp Int
  | Jie Char Int
  | Jio Char Int
  deriving (Eq, Show)

-- TODO: move into lib (shared with y2015d16)
eraseChars :: String -> String -> String
eraseChars elim = filter (not . (`elem` elim))

loudRead :: (Read a) => String -> a
loudRead s = case readMaybe s of
  Just x -> x
  Nothing -> error $ "Unable to parse '" ++ s ++ "'"

parseLine :: String -> Instr
parseLine str = case words (eraseChars "+," str) of
  ["hlf", [r]] -> Hlf r
  ["tpl", [r]] -> Tpl r
  ["inc", [r]] -> Inc r
  ["jmp", offset] -> Jmp (loudRead offset)
  ["jie", [r], offset] -> Jie r (loudRead offset)
  ["jio", [r], offset] -> Jio r (loudRead offset)
  _ -> error $ "Unable to parse " ++ str

type State = (Int, M.Map Char Int)

exec :: Instr -> State -> State
exec (Hlf r) (n, regs) = (n + 1, M.adjust (`div` 2) r regs)
exec (Tpl r) (n, regs) = (n + 1, M.adjust (* 3) r regs)
exec (Inc r) (n, regs) = (n + 1, M.adjust (+ 1) r regs)
exec (Jmp offset) (n, regs) = (n + offset, regs)
exec (Jie r offset) (n, regs) = (if even (regs M.! r) then n + offset else n + 1, regs)
exec (Jio r offset) (n, regs) = (if (regs M.! r) == 1 then n + offset else n + 1, regs)

(!?) :: (Ord t, Num t) => [a] -> t -> Maybe a
[] !? _ = Nothing
(x : xs) !? i
  | i < 0 = Nothing
  | i == 0 = Just x
  | otherwise = xs !? (i - 1)

-- TODO: is there a vector type for fast indexing?
step :: [Instr] -> State -> Maybe State
step instrs s@(idx, _) = fmap (`exec` s) (instrs !? idx)

-- TODO: I think this is a fold
iterateToNone :: (a -> Maybe a) -> a -> [a]
iterateToNone fn start = go (Just start)
  where
    go Nothing = []
    go (Just x) = x : go (fn x)

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let instrs = map parseLine $ lines content
      initState = (0, M.fromList [('a', 0), ('b', 0)])
      initState2 = (0, M.fromList [('a', 1), ('b', 0)])
  print instrs
  print $ last $ iterateToNone (step instrs) initState
  print $ last $ iterateToNone (step instrs) initState2
