-- https://adventofcode.com/2015/day/15

import Data.List
import System.Environment (getArgs)

replace :: (Eq a) => a -> a -> [a] -> [a]
replace a b = map $ \c -> if c == a then b else c

-- Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
-- calories get moved to the head of the list
parseLine :: String -> (String, [Int])
parseLine str = case words (replace ':' ' ' $ replace ',' ' ' str) of
  [name, "capacity", capacity, "durability", durability, "flavor", flavor, "texture", texture, "calories", calories] ->
    (name, map read [calories, capacity, durability, flavor, texture])
  _ -> error $ "Unable to parse " ++ str

-- amounts 4 100 is all the of ways 4 numbers can add to 100 ([0,0,0,100], [0,0,1,99], etc.)
amounts :: Int -> Int -> [[Int]]
amounts 0 _ = [[]]
amounts 1 k = [[k]]
amounts n 0 = replicate n [0]
amounts n k = [0 .. k] >>= (\i -> map (i :) (amounts (n - 1) (k - i)))

-- mix all the ingredients
mix :: [[Int]] -> [Int] -> [Int]
mix ingredients tsps = map sum $ transpose $ zipWith (\ings tsp -> map (* tsp) ings) ingredients tsps

-- ingredients -> amounts -> score
score1 :: [[Int]] -> [Int] -> Int
score1 ingredients tsps = product $ map (max 0) $ tail $ mix ingredients tsps

hasCalories :: Int -> [[Int]] -> [Int] -> Bool
hasCalories n ingredients tsps = head (mix ingredients tsps) == n

pairWith :: (a -> b) -> a -> (b, a)
pairWith fn a = (fn a, a)

main :: IO ()
main = do
  args <- getArgs
  let inputFile = head args
  content <- readFile inputFile
  let namedIngredients = map parseLine $ lines content
      ingredients = map snd namedIngredients
      recipes = amounts (length ingredients) 100
      recipes2 = filter (hasCalories 500 ingredients) recipes
      part1 = maximum $ map (pairWith (score1 ingredients)) recipes
      part2 = maximum $ map (pairWith (score1 ingredients)) recipes2
  print ingredients
  print part1
  print part2
