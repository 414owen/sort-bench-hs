{-# LANGUAGE ScopedTypeVariables #-}

module Sorts.MergeSortTopDownWithVec
  ( sortBy
  ) where

import Data.Vector (Vector)
import qualified Data.Vector as V

-- | Simple out-of-place mergeSort
-- Creates a vector from the input for O(1) slicing
sortBy :: forall a. (a -> a -> Ordering) -> [a] -> [a]
sortBy f = go . V.fromList
  where
    go :: Vector a -> [a]
    go v
      | V.length v < 2 = V.toList v
      | otherwise      = let (l, r) = splitVec v in mergeBy f (go l) (go r)

splitVec :: Vector a -> (Vector a, Vector a)
splitVec v = let n = V.length v `div` 2 in
  (V.take n v, V.drop n v)

mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy f l@(x : xs) r@(y : ys) = case f x y of
  LT -> x : mergeBy f xs r
  GT -> y : mergeBy f l ys
  EQ -> x : mergeBy f xs r
mergeBy _ [] xs = xs
mergeBy _ xs [] = xs
