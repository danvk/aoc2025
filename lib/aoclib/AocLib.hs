module AocLib (minUsing, maxUsing) where

import Data.Function
import Data.List

minUsing :: (Ord b) => (a -> b) -> [a] -> a
minUsing fn = minimumBy (compare `on` fn)

maxUsing :: (Ord b) => (a -> b) -> [a] -> a
maxUsing fn = maximumBy (compare `on` fn)
