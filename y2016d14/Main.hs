-- https://adventofcode.com/2016/day/14

import Crypto.Hash.MD5 (hash)
import Data.ByteString.Base16
import Data.ByteString.Char8 qualified as B
import Data.List
import Data.Maybe
import System.Environment (getArgs)

saltedMd5 :: String -> Int -> String
saltedMd5 salt x = md5str $ salt ++ show x

md5str :: String -> String
md5str = B.unpack . encode . hash . B.pack

-- TODO: try writing this point-free
stretchedHash :: String -> Int -> String
stretchedHash salt n = iterate md5str (saltedMd5 salt n) !! 2016

findTriple :: String -> Maybe Char
findTriple (a : b : c : _)
  | a == b && b == c = Just a
findTriple (_ : xs) = findTriple xs
findTriple [] = Nothing

-- firstJust :: (a -> Maybe b) -> [a] -> Maybe (a, b)
-- firstJust _ [] = Nothing
-- firstJust f (x : xs) = case f x of
--   (Just y) -> Just (x, y)
--   Nothing -> firstJust f xs

filterToKeys :: [(Int, String)] -> [(Int, String)]
filterToKeys [] = []
filterToKeys ((i, x) : xs) = case findTriple x of
  Nothing -> filterToKeys xs
  Just c ->
    let next1000 = map snd $ take 1000 xs
        target = [c, c, c, c, c]
     in if isJust $ find (\h -> target `isInfixOf` h) next1000 then (i, x) : filterToKeys xs else filterToKeys xs

main :: IO ()
main = do
  args <- getArgs
  let salt = head args
  let hashes = zip [0 ..] $ map (saltedMd5 salt) [0 ..]
      part1 = filterToKeys hashes !! 63
  print part1
  let hashes2 = zip [0 ..] $ map (stretchedHash salt) [0 ..]
      part2 = filterToKeys hashes2 !! 63
  print part2
