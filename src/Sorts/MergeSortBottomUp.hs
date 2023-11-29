{-# LANGUAGE ScopedTypeVariables #-}

module Sorts.MergeSortBottomUp
  ( sortBy
  ) where

import qualified Data.List as List

sortBy :: forall a. (a -> a -> Ordering) -> [a] -> [a]
sortBy f = mergeSort f . fmap List.singleton

mergeSort :: (a -> a -> Ordering) -> [[a]] -> [a]
mergeSort f xs@(_ : _ : _) = mergeSort f (mergePairs f xs)
mergeSort _ [xs] = xs
mergeSort _ [] = []

mergePairs :: (a -> a -> Ordering) -> [[a]] -> [[a]]
mergePairs f (x1 : x2 : xs) = mergeBy f x1 x2 : mergePairs f xs
mergePairs _ [xs] = [xs]
mergePairs _ []   = []

mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy f l@(x : xs) r@(y : ys) = case f x y of
  LT -> x : mergeBy f xs r
  GT -> y : mergeBy f l ys
  EQ -> x : y : mergeBy f xs ys
mergeBy _ _ _ = []
