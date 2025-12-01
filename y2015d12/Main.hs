{-# LANGUAGE OverloadedStrings #-}

-- https://adventofcode.com/2015/day/12
import System.Environment (getArgs)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as B
import Data.Scientific

sumJson :: Value -> Scientific
sumJson (Object o) = sum $ map sumJson $ KM.elems o
sumJson (Number n) = n
sumJson (Array a) = sum $ map sumJson $ V.toList a
sumJson _ = 0

sumJsonNoRed :: Value -> Scientific
sumJsonNoRed (Object o)
    | String "red" `elem` o = 0
    | otherwise = sum $ map sumJsonNoRed $ KM.elems o
sumJsonNoRed (Number n) = n
sumJsonNoRed (Array a) = sum $ map sumJsonNoRed $ V.toList a
sumJsonNoRed _ = 0

main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args
    content <- B.readFile inputFile
    let maybeVal = decode content :: Maybe Value
        val = case maybeVal of
            Nothing -> error "Could not parse"
            Just v -> v
        part1 = sumJson val
        part2 = sumJsonNoRed val
    print part1
    print part2
