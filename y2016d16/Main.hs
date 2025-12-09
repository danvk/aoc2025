-- https://adventofcode.com/2016/day/16

import Data.List
import Data.Maybe
import System.Environment (getArgs)

dragon :: String -> String
dragon a = a ++ ['0'] ++ b
  where
    b = [if c == '0' then '1' else '0' | c <- reverse a]

checksumEven :: String -> String
checksumEven [] = []
checksumEven (a : b : xs) = (if a == b then '1' else '0') : checksumEven xs
checksumEven _ = error "odd"

checksum :: String -> String
checksum s = if odd $ length s then s else checksum (checksumEven s)

main :: IO ()
main = do
  args <- getArgs
  let input = head args
      diskSize = (read @Int) (args !! 1)
      content = take diskSize $ fromJust $ find (\x -> length x >= diskSize) $ iterate dragon input
      part1 = checksum content

  print part1
