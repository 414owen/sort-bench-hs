{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OtherSorts
  ( runs
  , calcAggregates
  ) where

import Control.DeepSeq (NFData)
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)

import qualified Data.List   as List

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

{-
mergeSortByWithHeuristics :: forall a. (a -> a -> Ordering) -> [a] -> [a]
mergeSortByWithHeuristics f list
  = let (l, r) = splitList list in
  mergeBy f (undefined f l) (mergeSortByList f r)

  where
    splitList :: [a] -> ([a], [a])
    splitList = splitList' ([], [])

    splitList' :: ([a], [a]) -> [a] -> ([a], [a])
    splitList' (outX, outY) inputs = case inputs of
      (x1 : x2 : xs) -> splitList' (x1 : outX, x2 : outY) xs
      [x] -> (x : outX, outY)
      [] -> (outX, outY)
-}
