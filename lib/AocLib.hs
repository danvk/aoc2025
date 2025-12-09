module AocLib (minUsing, maxUsing, eraseChars, loudRead, mapReduce, groupByFn, binarySearch, lowerBound) where

import Control.Applicative
import Data.Function
import Data.List
import Text.Read (readMaybe)

minUsing :: (Ord b) => (a -> b) -> [a] -> a
minUsing fn = minimumBy (compare `on` fn)

maxUsing :: (Ord b) => (a -> b) -> [a] -> a
maxUsing fn = maximumBy (compare `on` fn)

eraseChars :: String -> String -> String
eraseChars elim = filter (not . (`elem` elim))

loudRead :: (Read a) => String -> a
loudRead s = case readMaybe s of
  Just x -> x
  Nothing -> error $ "Unable to parse '" ++ s ++ "'"

groupByFn :: (Ord b) => (a -> b) -> [a] -> [(b, [a])]
groupByFn keyFn = mapReduce (\x -> [(keyFn x, x)]) (\_ x -> x)

mapReduce :: (Ord b) => (a -> [(b, c)]) -> (b -> [c] -> d) -> [a] -> [(b, d)]
mapReduce mapFn reduceFn xs = merged
  where
    mapped = concatMap mapFn xs
    grouped = groupBy (\a b -> fst a == fst b) $ sortBy (compare `on` fst) mapped
    merged = map (\pcs -> (fst $ head pcs, reduceFn (fst $ head pcs) (map snd pcs))) grouped

-- Find an element x such that compareFn x == EQ using binary search.
binarySearch :: (Integral a) => (a -> Ordering) -> (a, a) -> Maybe a
binarySearch compareFn (x, y)
  | x == y = if compareFn x == EQ then Just x else Nothing
  | y < x = Nothing
binarySearch compareFn (x, y) =
  let mid = x + ((y - x) `div` 2)
   in case compareFn mid of
        LT -> binarySearch compareFn (x, mid - 1)
        GT -> binarySearch compareFn (mid + 1, y)
        EQ -> Just mid

-- Find the smallest x such that compareFn x == EQ using binary search
lowerBound :: (Integral a) => (a -> Ordering) -> (a, a) -> Maybe a
lowerBound compareFn (x, y)
  | x > y = Nothing
  | x == y = if compareFn x == EQ then Just x else Nothing
  | otherwise =
      let mid = x + ((y - x) `div` 2)
       in case compareFn mid of
            LT -> lowerBound compareFn (mid + 1, y)
            GT -> lowerBound compareFn (x, mid - 1)
            EQ -> lowerBound compareFn (x, mid - 1) <|> Just mid
