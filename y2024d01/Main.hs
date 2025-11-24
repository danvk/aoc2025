import Data.List (sort)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)

main = do
    args <- getArgs
    let inputFile = head args
    content <- readFile inputFile
    let numbers = map words (lines content)
    let list1 = map (read . head) numbers
    let list2 = map (read . (!!1)) numbers
    let pairs = zip (sort list1) (sort list2)
    let deltas = map (\(a, b) -> abs (a - b)) pairs
    let answer1 = sum deltas
    print answer1
    let counts = Map.fromListWith (+) (zip list2 (repeat 1))
    let answer2 = sum $ fmap (\x -> x * (fromMaybe 0 (Map.lookup x counts))) list1
    print answer2
