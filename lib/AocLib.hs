module AocLib (minUsing, maxUsing, eraseChars, loudRead, mapReduce) where

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

mapReduce :: (Ord b) => (a -> [(b, c)]) -> (b -> [c] -> d) -> [a] -> [(b, d)]
mapReduce mapFn reduceFn xs = merged
  where
    mapped = concatMap mapFn xs
    grouped = groupBy (\a b -> fst a == fst b) $ sortBy (compare `on` fst) mapped
    merged = map (\pcs -> (fst $ head pcs, reduceFn (fst $ head pcs) (map snd pcs))) grouped
