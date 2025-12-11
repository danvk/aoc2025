-- https://adventofcode.com/2016/day/23

import AocLib
import Data.List
import Data.Map.Strict qualified as M
import Debug.Trace
import System.Environment (getArgs)
import Text.Read (readMaybe)

data RegOrInt = Reg Char | Const Int deriving (Eq, Show)

readRegOrInt :: String -> RegOrInt
readRegOrInt s = case readMaybe s of
  Just n -> Const n
  Nothing -> Reg (head s)

data Instr
  = Cpy RegOrInt RegOrInt
  | Inc RegOrInt
  | Dec RegOrInt
  | Jnz RegOrInt RegOrInt
  | Tgl RegOrInt
  deriving (Eq, Show)

parseLine :: String -> Instr
parseLine str = case words (eraseChars "+," str) of
  ["inc", r] -> Inc (readRegOrInt r)
  ["dec", r] -> Dec (readRegOrInt r)
  ["cpy", src, dst] -> Cpy (readRegOrInt src) (readRegOrInt dst)
  ["jnz", src, offset] -> Jnz (readRegOrInt src) (readRegOrInt offset)
  ["tgl", src] -> Tgl (readRegOrInt src)
  _ -> error $ "Unable to parse " ++ str

(!?) :: (Ord t, Num t) => [a] -> t -> Maybe a
[] !? _ = Nothing
(x : xs) !? i
  | i < 0 = Nothing
  | i == 0 = Just x
  | otherwise = xs !? (i - 1)

type State = (Int, M.Map Char Int, [Instr])

getRegOrInt :: M.Map Char Int -> RegOrInt -> Int
getRegOrInt _ (Const n) = n
getRegOrInt m (Reg r) = m M.! r

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex i newValue xs = take i xs ++ [newValue] ++ drop (i + 1) xs

toggleInstr :: Instr -> Instr
toggleInstr (Inc x) = Dec x
toggleInstr (Dec x) = Inc x
toggleInstr (Tgl x) = Inc x
toggleInstr (Cpy a b) = Jnz a b
toggleInstr (Jnz a b) = Cpy a b

exec :: Instr -> State -> State
exec (Inc (Reg r)) (n, regs, instrs) = (n + 1, M.adjust (+ 1) r regs, instrs)
exec (Inc (Const _)) (n, regs, instrs) = (n + 1, regs, instrs)
exec (Dec (Reg r)) (n, regs, instrs) = (n + 1, M.adjust (+ (-1)) r regs, instrs)
exec (Dec (Const _)) (n, regs, instrs) = (n + 1, regs, instrs)
exec (Cpy src (Reg r)) (n, regs, instrs) = (n + 1, M.insert r (getRegOrInt regs src) regs, instrs)
exec (Cpy _ (Const _)) (n, regs, instrs) = (n + 1, regs, instrs) -- invalid; no-op
exec (Jnz src dst) (n, regs, instrs) = (if getRegOrInt regs src /= 0 then n + getRegOrInt regs dst else n + 1, regs, instrs)
exec (Tgl offset) s@(n, regs, instrs) =
  let idx = trace (show s) n + getRegOrInt regs offset
      newInstrs = case instrs !? idx of
        Just instr -> replaceAtIndex idx (toggleInstr instr) instrs
        Nothing -> instrs
   in (n + 1, regs, newInstrs)

step :: State -> Maybe State
step s@(idx, _, instrs) = fmap (`exec` s) (instrs !? idx)

iterateToNone :: (a -> Maybe a) -> a -> [a]
iterateToNone f = unfoldr (\x -> (x,) <$> f x)

snd3 :: (a, b, c) -> b
snd3 (_, a, _) = a

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let instrs = map parseLine $ lines content
      initState = (0, M.fromList [('a', 12), ('b', 0), ('c', 0), ('d', 0)], instrs)
      part1 = snd3 $ last $ iterateToNone step initState
  print part1
