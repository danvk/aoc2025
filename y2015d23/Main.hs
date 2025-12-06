-- https://adventofcode.com/2015/day/23
import System.Environment (getArgs)
import Text.Read

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

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let x = map parseLine $ lines content
  print x
