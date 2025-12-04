-- https://adventofcode.com/2015/day/19
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import AocLib
import Data.Char
import Data.List
import Data.List.Split
import Data.Map.Strict qualified as M
import System.Environment (getArgs)

parseReplacement :: String -> (String, [String])
parseReplacement str =
  let [left, right] = splitOn " => " str
   in (left, parseMolecule right)

parseMolecule :: String -> [String]
parseMolecule "" = []
parseMolecule (x : y : xs)
  | isUpperCase x && isLowerCase y = [x, y] : parseMolecule xs
parseMolecule (x : xs) = [x] : parseMolecule xs

indexReplacements :: [(String, [String])] -> M.Map String [[String]]
indexReplacements reps = M.fromListWith (++) $ map (\(lhs, rhs) -> (lhs, [rhs])) reps

applyReplacement :: M.Map String [[String]] -> [String] -> [[String]]
applyReplacement _ [] = []
applyReplacement repMap (x : xs) =
  if x `M.member` repMap
    then
      ( let replaceIt = [rep ++ xs | rep <- repMap M.! x]
         in let dontReplaceIt = map (x :) $ applyReplacement repMap xs
             in replaceIt ++ dontReplaceIt
      )
    -- TODO: dedupe this clause
    else map (x :) $ applyReplacement repMap xs

step :: M.Map String [[String]] -> [[String]] -> [[String]]
step repMap molecules = nub $ concatMap (applyReplacement repMap) molecules

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let inLines = lines content
      splitted = splitOn [""] inLines
      [replacementStrs, [inputMoleculeStr]] = splitted
      replacements = indexReplacements $ map parseReplacement replacementStrs
      inputMolecule = parseMolecule inputMoleculeStr
      part1 = length $ step replacements [inputMolecule]
  print replacements
  print inputMolecule
  print part1

  let initState = [["e"]]
      states = take 7 $ iterate (step replacements) initState
      longest = maxUsing length (last states)
  print $ map length states
  print longest
  print $ length longest
