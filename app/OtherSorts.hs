{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OtherSorts
  ( mergeSortWithVec
  , mergeSortWithList
  , mergeSortOnBottomUp
  , sortWith'
  , runs
  , calcAggregates
  ) where

import Control.DeepSeq (NFData)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import GHC.Generics (Generic)

import Data.Vector (Vector)
import qualified Data.List   as List
import qualified Data.Vector as V

-- | Simple top-down mergeSort
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

data Aggregates
  = Aggregates
  { numAscRuns      :: {-# UNPACK #-} !Int
  , numDescRuns     :: {-# UNPACK #-} !Int
  , avgRunLength :: {-# UNPACK #-} !Int
  , elAmt        :: {-# UNPACK #-} !Int
  } deriving (Generic, NFData)

runs :: forall a. (a -> a -> Ordering) -> [a] -> [(Ordering, Int)]
runs f (x1 : x2 : xs) = case f x1 x2 of
  LT -> let (amt, rest) = ascRun 2 x2 xs in (LT, amt) : runs f rest
  GT -> let (amt, rest) = descRun 2 x2 xs in (GT, amt) : runs f rest
  EQ -> case runs f (x2 : xs) of
    ((order, n) : rest) -> (order, succ n) : rest
    [] -> error "impossible!"
  where
    ascRun :: Int -> a -> [a] -> (Int, [a])
    ascRun !amt !lastEl (!y1 : ys)
      | f lastEl y1 < GT = ascRun (succ amt) y1 ys
      | otherwise = (amt, ys)
    ascRun _ _ _ = (0, [])

    descRun :: Int -> a -> [a] -> (Int, [a])
    descRun !amt !lastEl (!y1 : ys)
      | f lastEl y1 > LT = descRun (succ amt) y1 ys
      | otherwise = (amt, ys)
    descRun _ _ _ = (0, [])
runs _ [_] = [(EQ, 1)]
runs _ [] = []

calcAggregates :: forall a. (a -> a -> Ordering) -> [a] -> Aggregates
calcAggregates f xs
  = Aggregates
  { numAscRuns = length ascRuns
  , numDescRuns = length descRuns
  , avgRunLength = (sum ascRuns + sum descRuns) `div` max 1 (length ascRuns + length descRuns)
  , elAmt = List.foldl' (\acc (_, n) -> acc + n) 0 runs'
  }
  where
    runs' :: [(Ordering, Int)]
    runs' = runs f xs

    ascRuns :: [Int]
    ascRuns = mapMaybe runToAscRun runs'
    
    descRuns :: [Int]
    descRuns = mapMaybe runToDescRun runs'

runToAscRun :: (Ordering, Int) -> Maybe Int
runToAscRun (LT, n) = Just n
runToAscRun _ = Nothing

runToDescRun :: (Ordering, Int) -> Maybe Int
runToDescRun (GT, n) = Just n
runToDescRun _ = Nothing

mergeSortByWithHeuristics :: forall a. (a -> a -> Ordering) -> [a] -> [a]
mergeSortByWithHeuristics f list
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

mergeSortByBottomUp :: forall a. (a -> a -> Ordering) -> [a] -> [a]
mergeSortByBottomUp f = mergeSort . fmap List.singleton
  where
    mergeSort :: [[a]] -> [a]
    mergeSort xs@(_ : _ : _) = mergeSort (mergePairs xs)
    mergeSort [xs] = xs
    mergeSort [] = []

    mergePairs :: [[a]] -> [[a]]
    mergePairs (x1 : x2 : xs) = mergeBy f x1 x2 : mergePairs xs
    mergePairs [xs] = [xs]
    mergePairs []   = []

mergeSortOnBottomUp :: Ord b => (a -> b) -> [a] -> [a]
mergeSortOnBottomUp f = mergeSortByBottomUp $ comparing f

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
mergeBy f l@(x : xs) r@(y : ys) = case f x y of
  LT -> x : mergeBy f xs r
  GT -> y : mergeBy f l ys
  EQ -> x : y : mergeBy f xs ys
mergeBy _ _ _ = []

mergeSortWithVec :: Ord b => (a -> b) -> [a] -> [a]
mergeSortWithVec f = mergeSortByVec $ comparing f

mergeSortWithList :: Ord b => (a -> b) -> [a] -> [a]
mergeSortWithList f = mergeSortByList $ comparing f

-- Like 'GHC.Exts.sortWith', but using 'comparing' instead of
-- an inline function
sortWith' :: Ord b => (a -> b) -> [a] -> [a]
sortWith' = sortBy . comparing
