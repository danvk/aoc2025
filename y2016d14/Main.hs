-- https://adventofcode.com/2016/day/14

import Crypto.Hash.MD5 (hash)
import Data.ByteString.Base16
import Data.ByteString.Char8 qualified as B
import System.Environment (getArgs)

saltedMd5 :: String -> Int -> B.ByteString
saltedMd5 salt x = encode $ hash $ B.pack $ salt ++ show x

main :: IO ()
main = do
  args <- getArgs
  let salt = head args
  let hashes = map (saltedMd5 salt) [0 ..]
  print $ hashes !! 18
