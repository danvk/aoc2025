-- https://adventofcode.com/2015/day/7
import System.Environment (getArgs)
import qualified Data.Map.Strict as Map
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Text.Read (readMaybe)

data Instruction = Instruction LHS String deriving (Show)
data LHS =
    Constant Int
    | ConstantReg String
    | And String String
    | AndConst Int String
    | LShift String Int
    | Not String
    | Or String String
    | RShift String Int deriving (Show)

-- 123 -> x
-- x AND y -> d
-- x OR y -> e
-- x LSHIFT 2 -> f
-- y RSHIFT 2 -> g
-- NOT y -> i

parseLine :: String -> Instruction
parseLine str = case words str of
    [input, "->", out] -> case readMaybe input of
        Just num -> Instruction (Constant num) out
        Nothing -> Instruction (ConstantReg input) out
    [input1, "AND", y, "->", out] -> case readMaybe input1 of
        Just x -> Instruction (AndConst x y) out
        Nothing -> Instruction (And input1 y) out
    [x, "OR", y, "->", out] -> Instruction (Or x y) out
    [x, "LSHIFT", y, "->", out] -> Instruction (LShift x (read y)) out
    [x, "RSHIFT", y, "->", out] -> Instruction (RShift x (read y)) out
    ["NOT", x, "->", out] -> Instruction (Not x ) out
    _ -> error $ "Invalid line: " ++ str

indexInstructions :: [Instruction] -> Map.Map String LHS
indexInstructions instrs = Map.fromList [(out, lhs) | Instruction lhs out <- instrs ]

getRegister :: Map.Map String LHS -> String -> Int
getRegister m reg = fst (go Map.empty reg)
  where
    go cache r = case Map.lookup r cache of
      Just val -> (val, cache)
      Nothing -> case Map.lookup r m of
        Just cmd -> case cmd of
          (Constant x) -> (x, Map.insert r x cache)
          (ConstantReg r') ->
            let (val, cache') = go cache r'
            in (val, Map.insert r val cache')
          (And x y) ->
            let (val1, cache1) = go cache x
                (val2, cache2) = go cache1 y
                val = val1 .&. val2
            in (val, Map.insert r val cache2)
          (AndConst x y) ->
            let (val, cache') = go cache y
                result = x .&. val
            in (result, Map.insert r result cache')
          (Or x y) ->
            let (val1, cache1) = go cache x
                (val2, cache2) = go cache1 y
                val = val1 .|. val2
            in (val, Map.insert r val cache2)
          (Not x) ->
            let (val, cache') = go cache x
                result = 65535 - val
            in (result, Map.insert r result cache')
          (LShift x n) ->
            let (val, cache') = go cache x
                result = shiftL val n
            in (result, Map.insert r result cache')
          (RShift x n) ->
            let (val, cache') = go cache x
                result = shiftR val n
            in (result, Map.insert r result cache')
        Nothing -> error $ "Missing register " ++ r


main :: IO ()
main = do
    args <- getArgs
    let [inputFile, register] = args
    content <- readFile inputFile
    let instrs = map parseLine $ lines content
        instrMap = indexInstructions instrs
    print $ Map.toList instrMap
    print $ getRegister instrMap register
