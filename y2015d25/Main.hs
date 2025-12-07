-- https://adventofcode.com/2015/day/25

import Data.List
import System.Environment (getArgs)

assertJust :: Maybe a -> a
assertJust (Just x) = x
assertJust Nothing = error "nothing"

coords :: [(Int, Int)]
coords = go (1, 1)
  where
    go :: (Int, Int) -> [(Int, Int)]
    go (x, 1) = (x, 1) : go (1, x + 1)
    go (x, y) = (x, y) : go (x + 1, y - 1)

main :: IO ()
main = do
  args <- getArgs
  let row = read @Int $ head args
      col = read @Int $ args !! 1
  -- print row
  -- print col
  -- print $ take 10 coords
  print $ 1 + assertJust (elemIndex (col, row) coords)
