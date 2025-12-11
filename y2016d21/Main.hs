-- https://adventofcode.com/2016/day/21

import Data.List
import Data.Maybe
import System.Environment (getArgs)

data Op
  = SwapPos Int Int
  | SwapLet Char Char
  | RotateLeft Int
  | RotateRight Int
  | RotatePos Char
  | Reverse Int Int
  | Move Int Int
  deriving (Eq, Show)

parseLine :: String -> Op
parseLine str = case words str of
  ["swap", "position", p1, "with", "position", p2] -> SwapPos (read p1) (read p2)
  ["swap", "letter", [a], "with", "letter", [b]] -> SwapLet a b
  ["reverse", "positions", p1, "through", p2] -> Reverse (read p1) (read p2)
  ["rotate", "right", n, _] -> RotateLeft (read n)
  ["rotate", "left", n, _] -> RotateRight (read n)
  ["rotate", "based", "on", "position", "of", "letter", [c]] -> RotatePos c
  ["move", "position", p1, "to", "position", p2] -> Move (read p1) (read p2)
  _ -> error $ "Unable to parse " ++ str

applyOp :: String -> Op -> String
applyOp s (SwapPos a b)
  | a < b =
      let (first, rest) = splitAt a s
          (letA : restA) = rest
          (middle, restB) = splitAt (b - a - 1) restA
          (letB : end) = restB
       in first ++ [s !! b] ++ middle ++ [s !! a] ++ end
  | a > b = applyOp s (SwapPos b a)
  | otherwise = s
applyOp s (SwapLet a b) = applyOp s (SwapPos pa pb)
  where
    pa = fromJust $ a `elemIndex` s
    pb = fromJust $ b `elemIndex` s
applyOp s (RotateLeft r) = back ++ front
  where
    (front, back) = splitAt r s
applyOp s (RotateRight r) = back ++ front
  where
    (front, back) = splitAt (length s - r) s
applyOp s (Reverse a b)
  | a < b =
      let (first, rest) = splitAt a s
          (middle, end) = splitAt (b - a + 1) rest
       in first ++ reverse middle ++ end
  | a > b = applyOp s (Reverse b a)
  | otherwise = s

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let x = map parseLine $ lines content
  print x
