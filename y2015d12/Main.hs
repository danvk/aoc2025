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


main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args
    content <- readFile inputFile
    let val = decode $ strictByteString content :: Maybe Value
        part1 = case val of
            Nothing -> error "Could not parse"
            Just v -> sumJson v
    print part1
