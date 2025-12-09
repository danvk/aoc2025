-- https://adventofcode.com/2016/day/21
import System.Environment (getArgs)

parseLine :: String -> [String]
parseLine str = case words str of
  word:rest -> word:rest
  _ -> error $ "Unable to parse " ++ str

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let x = map parseLine $ lines content
  print x
