-- https://adventofcode.com/2015/day/4

import Crypto.Hash.MD5 (hash)
import Data.ByteString.Base16
import Data.ByteString.Char8 qualified as B
import System.Environment (getArgs)

saltedMd5 :: String -> Int -> B.ByteString
saltedMd5 salt x = encode $ hash $ B.pack $ salt ++ show x

isWinner :: String -> Int -> Bool
isWinner salt x =
  let h = B.unpack (saltedMd5 salt x)
   in case h of
        '0' : '0' : '0' : '0' : '0' : _ -> True
        _ -> False

isWinner2 :: String -> Int -> Bool
isWinner2 salt x =
  let h = B.unpack (saltedMd5 salt x)
   in case h of
        '0' : '0' : '0' : '0' : '0' : '0' : _ -> True
        _ -> False

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let prefix = head $ lines content
  print prefix
  let part1 = head [n | n <- [1 ..], isWinner prefix n]
  print part1
  let part2 = head [n | n <- [1 ..], isWinner2 prefix n]
  print part2
