{-# LANGUAGE ScopedTypeVariables #-}

module OtherSorts where

import Data.List (sortBy)
import Data.Ord (comparing)

import Data.Vector (Vector)
import qualified Data.Vector as V

mergeSortBy :: forall a. (a -> a -> Ordering) -> [a] -> [a]
mergeSortBy f lst = go $ V.fromList lst
  where
    go :: Vector a -> [a]
    go v
      | V.length v < 2 = V.toList v
      | otherwise      = let (l, r) = splitVec v in merge (go l) (go r)

    splitVec :: Vector a -> (Vector a, Vector a)
    splitVec v = let n = V.length v `div` 2 in
      (V.take n v, V.drop n v)

    merge :: [a] -> [a] -> [a]
    merge [] xs = xs
    merge xs [] = xs
    merge l@(x : xs) r@(y : ys) = case f x y of
      LT -> x : merge xs r
      GT -> y : merge l ys
      EQ -> x : y : merge xs ys

mergeSortWith :: Ord b => (a -> b) -> [a] -> [a]
mergeSortWith f = mergeSortBy $ comparing f

-- Like 'GHC.Exts.sortWith', but using 'comparing' instead of
-- an inline function
sortWith' :: Ord b => (a -> b) -> [a] -> [a]
sortWith' = sortBy . comparing
