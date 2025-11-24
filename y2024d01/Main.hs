import Data.List (sort)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)

main = do
    args <- getArgs
    let inputFile = head args
    content <- readFile inputFile
    let numbers = map words (lines content)
    let list1 = fmap (read . head) numbers
    let list2 = fmap (read . (!!1)) numbers
    let answer1 = sum $ zipWith (\a b -> abs $ subtract a b) (sort list1) (sort list2)
    print answer1
    let counts = Map.fromListWith (+) (zip list2 (repeat 1))
    let answer2 = sum $ fmap (\x -> x * (fromMaybe 0 (Map.lookup x counts))) list1
    print answer2
