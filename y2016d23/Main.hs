-- https://adventofcode.com/2016/day/23

import AocLib
import Data.Map.Strict qualified as M
import Debug.Trace
import System.Environment (getArgs)
import Text.Read

data RegOrInt = Reg Char | Const Int deriving (Eq, Show)

readRegOrInt :: String -> RegOrInt
readRegOrInt s = case readMaybe s of
  Just n -> Const n
  Nothing -> Reg (head s)

data Instr
  = Cpy RegOrInt Char
  | Inc Char
  | Dec Char
  | Jnz RegOrInt RegOrInt
  | Tgl RegOrInt
  deriving (Eq, Show)

parseLine :: String -> Instr
parseLine str = case trace str words (eraseChars "+," str) of
  ["inc", [r]] -> Inc r
  ["dec", [r]] -> Dec r
  ["cpy", src, [r]] -> Cpy (readRegOrInt src) r
  ["jnz", src, offset] -> Jnz (readRegOrInt src) (readRegOrInt offset)
  ["tgl", src] -> Tgl (readRegOrInt src)
  _ -> error $ "Unable to parse " ++ str

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let x = map parseLine $ lines content
  print x
