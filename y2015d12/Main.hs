-- https://adventofcode.com/2015/day/12
import System.Environment (getArgs)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL (ByteString, fromStrict)
import Data.Text.Encoding (encodeUtf8)
import Data.Text as T (pack)
import Data.Scientific

strictByteString :: String -> BL.ByteString
strictByteString x = BL.fromStrict $ encodeUtf8 $ T.pack x

sumJson :: Value -> Int
sumJson (Object o) = sum $ map sumJson $ KM.elems o
sumJson (Number n) = case toBoundedInteger n of
    Just i -> i
    Nothing -> error "Bad number"
sumJson (Array a) = sum $ map sumJson $ V.toList a
sumJson _ = 0

sumJsonNoRed :: Value -> Int
sumJsonNoRed (Object o) =
    let vals = KM.elems o in
    if String (T.pack "red") `elem` vals then 0 else sum $ map sumJsonNoRed vals
sumJsonNoRed (Number n) = case toBoundedInteger n of
    Just i -> i
    Nothing -> error "Bad number"
sumJsonNoRed (Array a) = sum $ map sumJsonNoRed $ V.toList a
sumJsonNoRed _ = 0


main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args
    content <- readFile inputFile
    let maybeVal = decode $ strictByteString content :: Maybe Value
        val = case maybeVal of
            Nothing -> error "Could not parse"
            Just v -> v
        part1 = sumJson val
        part2 = sumJsonNoRed val
    print part1
    print part2
