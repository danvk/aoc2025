-- https://adventofcode.com/2015/day/7
import System.Environment (getArgs)
import qualified Data.Map.Strict as Map
import Data.Bits ((.&.), (.|.), complement, shiftL, shiftR)

data Instruction = Instruction LHS String
data LHS =
    Constant Int
    | And String String
    | LShift String Int
    | Not String
    | Or String String
    | RShift String Int

-- 123 -> x
-- x AND y -> d
-- x OR y -> e
-- x LSHIFT 2 -> f
-- y RSHIFT 2 -> g
-- NOT y -> i

parseLine :: String -> Instruction
parseLine str = case words str of
    [num, "->", out] -> Instruction (Constant $ read num) out
    [x, "AND", y, "->", out] -> Instruction (And x y) out
    [x, "OR", y, "->", out] -> Instruction (Or x y) out
    [x, "LSHIFT", y, "->", out] -> Instruction (LShift x (read y)) out
    [x, "RSHIFT", y, "->", out] -> Instruction (RShift x (read y)) out
    ["NOT", x, "->", out] -> Instruction (Not x ) out
    _ -> error $ "Invalid line: " ++ str

type RegMap = Map.Map String Int

runInstr :: RegMap -> LHS -> Int
runInstr _ (Constant x) = x
runInstr m (And x y) = (m Map.! x) .&. (m Map.! y)
runInstr m (Or x y) = (m Map.! x) .|. (m Map.! y)
runInstr m (Not x) = complement (m Map.! x)
runInstr m (LShift r n) = shiftL (m Map.! r) n
runInstr m (RShift r n) = shiftR (m Map.! r) n

applyInstr :: RegMap -> Instruction -> RegMap
applyInstr m (Instruction lhs out) = Map.insert out (runInstr m lhs) m

main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args
    content <- readFile inputFile
    let instrs = map parseLine $ lines content
        registers = foldl applyInstr Map.empty instrs
    -- print registers
    print $ registers Map.! "a"
