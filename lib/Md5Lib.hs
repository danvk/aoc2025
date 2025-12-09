module Md5Lib (md5str) where

import Crypto.Hash.MD5 (hash)
import Data.ByteString.Base16
import Data.ByteString.Char8 qualified as B

md5str :: String -> String
md5str = B.unpack . encode . hash . B.pack
