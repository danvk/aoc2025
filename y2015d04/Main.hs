-- https://adventofcode.com/2015/day/4
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import System.Environment (getArgs)
import Crypto.Hash.MD5 (hash)
import Data.ByteString.Base16

-- import Data.Hash.MD5
import qualified Data.ByteString.Char8 as B


salted_md5 :: String -> Int -> B.ByteString
salted_md5 salt x = encode $ hash $ B.pack $ salt ++ show x

is_winner :: String -> Int -> Bool
is_winner salt x =
    let h = B.unpack (salted_md5 salt x)
    in case h of
        '0':'0':'0':'0':'0':_ -> True
        _ -> False

is_winner2 :: String -> Int -> Bool
is_winner2 salt x =
    let h = B.unpack (salted_md5 salt x)
    in case h of
        '0':'0':'0':'0':'0':'0':_ -> True
        _ -> False

main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args
    content <- readFile inputFile
    let prefix = head $ lines content
    print prefix
    let part1 = head [n | n <- [1..], is_winner prefix n]
    print part1
    let part2 = head [n | n <- [1..], is_winner2 prefix n]
    print part2
