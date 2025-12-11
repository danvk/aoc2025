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
  ["rotate", "left", n, _] -> RotateLeft (read n)
  ["rotate", "right", n, _] -> RotateRight (read n)
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
    (front, back) = splitAt (r `mod` length s) s
applyOp s (RotateRight r) = back ++ front
  where
    (front, back) = splitAt (length s - (r `mod` length s)) s
applyOp s (Reverse a b)
  | a < b =
      let (first, rest) = splitAt a s
          (middle, end) = splitAt (b - a + 1) rest
       in first ++ reverse middle ++ end
  | a > b = applyOp s (Reverse b a)
  | otherwise = s
applyOp s (Move a b)
  | a < b =
      let (first, rest) = splitAt a s
          (letA : restA) = rest
          (middle, restB) = splitAt (b - a - 1) restA
          (letB : end) = restB
       in first ++ middle ++ [s !! b] ++ [s !! a] ++ end
  | a > b =
      let (first, rest) = splitAt b s
          (letB : restB) = rest
          (middle, restA) = splitAt (a - b - 1) restB
          (letA : end) = restA
       in first ++ [s !! a] ++ [s !! b] ++ middle ++ end
  | otherwise = s
applyOp s (RotatePos c) = applyOp s (RotateRight n)
  where
    n = 1 + idx + (if idx >= 4 then 1 else 0)
    idx = fromJust $ c `elemIndex` s

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
      pass0 = args !! 1
  content <- readFile inputFile
  let ops = map parseLine $ lines content
      part1 = foldl applyOp pass0 ops
  print part1
