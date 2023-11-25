{-# LANGUAGE NumericUnderscores #-}

module Main where

import Criterion.Main
import Data.Bifunctor
import Data.List
import Data.Ord
import Data.Tuple
import GHC.Exts (sortWith)

aList :: [Int]
aList = take 1_000 $ cycle $ concat $ permutations [0..20]

tupList :: [(Int, Int)]
tupList = zip (take 1_000 $ cycle $ concat $ permutations [0..20]) [0..]

type NestedTup a = (((((a, ()), ()), ()), ()), ())

nestedFst :: (NestedTup a, b) -> a
nestedFst = fst . fst . fst . fst . fst . fst

nestedEmbed :: a -> NestedTup a
nestedEmbed a = (((((a, ()), ()), ()), ()), ())

nestedTupList :: [(NestedTup Int, Int)]
nestedTupList = first nestedEmbed <$> zip (take 1_000 $ cycle $ concat $ permutations [0..20]) [0..]

main :: IO ()
main = defaultMain
  [ bench "sortOn id" $ nf (sortOn id) aList
  , bench "sortWith id" $ nf (sortWith id) aList
  , bench "sortBy (comparing id)" $ nf (sortBy $ comparing id) aList

  , bench "sortOn fst" $ nf (sortOn fst) tupList
  , bench "sortWith fst" $ nf (sortWith fst) tupList
  , bench "sortBy (comparing fst)" $ nf (sortBy $ comparing fst) tupList

  , bench "sortOn snd" $ nf (sortOn snd) $ swap <$> tupList
  , bench "sortWith snd" $ nf (sortWith snd) $ swap <$> tupList
  , bench "sortBy (comparing snd)" $ nf (sortBy $ comparing snd) $ swap <$> tupList

  , bench "sortOn nestedFst" $ nf (sortOn nestedFst) nestedTupList
  , bench "sortWith nestedFst" $ nf (sortWith nestedFst) nestedTupList
  , bench "sortBy (comparing nestedFst)" $ nf (sortBy $ comparing nestedFst) nestedTupList
  ]
