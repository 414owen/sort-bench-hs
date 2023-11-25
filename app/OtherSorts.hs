{-# LANGUAGE ScopedTypeVariables #-}

module OtherSorts
  ( mergeSortWithVec
  , mergeSortWithList
  , sortWith'
  ) where

import Data.List (sortBy)
import Data.Ord (comparing)

import Data.Vector (Vector)
import qualified Data.Vector as V

-- | Simple out-of-place mergeSort
-- Creates a vector from the input for O(1) slicing
mergeSortByList :: forall a. (a -> a -> Ordering) -> [a] -> [a]
mergeSortByList _ [] = []
mergeSortByList _ [a] = [a]
mergeSortByList f list
  = let (l, r) = splitList list in
  mergeBy f (mergeSortByList f l) (mergeSortByList f r)

  where
    splitList :: [a] -> ([a], [a])
    splitList = splitList' ([], [])
    
    splitList' :: ([a], [a]) -> [a] -> ([a], [a])
    splitList' (outX, outY) inputs = case inputs of
      (x1 : x2 : xs) -> splitList' (x1 : outX, x2 : outY) xs
      [x] -> (x : outX, outY)
      [] -> (outX, outY)

-- | Simple out-of-place mergeSort
-- Creates a vector from the input for O(1) slicing
mergeSortByVec :: forall a. (a -> a -> Ordering) -> [a] -> [a]
mergeSortByVec f lst = go $ V.fromList lst
  where
    go :: Vector a -> [a]
    go v
      | V.length v < 2 = V.toList v
      | otherwise      = let (l, r) = splitVec v in mergeBy f (go l) (go r)

    splitVec :: Vector a -> (Vector a, Vector a)
    splitVec v = let n = V.length v `div` 2 in
      (V.take n v, V.drop n v)

mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy _ [] xs = xs
mergeBy _ xs [] = xs
mergeBy f l@(x : xs) r@(y : ys) = case f x y of
  LT -> x : mergeBy f xs r
  GT -> y : mergeBy f l ys
  EQ -> x : y : mergeBy f xs ys

mergeSortWithVec :: Ord b => (a -> b) -> [a] -> [a]
mergeSortWithVec f = mergeSortByVec $ comparing f

mergeSortWithList :: Ord b => (a -> b) -> [a] -> [a]
mergeSortWithList f = mergeSortByList $ comparing f

-- Like 'GHC.Exts.sortWith', but using 'comparing' instead of
-- an inline function
sortWith' :: Ord b => (a -> b) -> [a] -> [a]
sortWith' = sortBy . comparing
