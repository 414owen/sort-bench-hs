module Sorts.MergeSortTopDownAlternating
  ( sortBy
  ) where

import Data.Function (on)
import Data.Word     (Word64)

import qualified Data.List as L

-- 1. zip with index
-- 2. unstable top-down mergesort
-- 3. groupWith equality
-- 4. unstable sort by index

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy f lst
  = fmap fst
  $ concatMap (sortBy' (on compare snd))
  $ L.groupBy (\(a, _) (b, _) -> f a b == EQ)
  $ sortBy' (on f fst)
  $ zip lst [0 :: Word64 ..]

-- | Simple top-down mergeSort
sortBy' :: (a -> a -> Ordering) -> [a] -> [a]
sortBy' _ [] = []
sortBy' _ [a] = [a]
sortBy' f list
  = let (l, r) = splitList list in
  mergeBy f (sortBy' f l) (sortBy' f r)

splitList :: [a] -> ([a], [a])
splitList = splitList' ([], [])

splitList' :: ([a], [a]) -> [a] -> ([a], [a])
splitList' (outX, outY) inputs = case inputs of
  (x1 : x2 : xs) -> splitList' (x1 : outX, x2 : outY) xs
  [x] -> (x : outX, outY)
  [] -> (outX, outY)

mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy f l@(x : xs) r@(y : ys) = case f x y of
  LT -> x : mergeBy f xs r
  GT -> y : mergeBy f l ys
  EQ -> x : y : mergeBy f xs ys
mergeBy _ [] xs = xs
mergeBy _ xs [] = xs
