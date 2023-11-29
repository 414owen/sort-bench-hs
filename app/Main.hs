{-# LANGUAGE NumericUnderscores  #-}

module Main
  ( main
  ) where

import Control.DeepSeq
import Criterion.Main
import Data.Ord (comparing)
import Data.List

import OtherSorts

import qualified Sorts.MergeSortTopDown        as MSTD
import qualified Sorts.MergeSortBottomUp       as MSBU
import qualified Sorts.MergeSortTopDownWithVec as MSTDV
import qualified Sorts.VectorSort              as VS

aList :: [Int]
aList = take 10_000 $ cycle $ concat $ permutations [0..20]

-- * Nested tuples

type RNestedTup a = ((), ((), ((), ((), ((), ((), ((), a)))))))

nestedSnd :: RNestedTup a -> a
nestedSnd = snd . snd . snd . snd . snd . snd . snd

embedSnd :: a -> ((), a)
embedSnd a = ((), a)

toNestedSnd :: a -> RNestedTup a
toNestedSnd = embedSnd . embedSnd . embedSnd . embedSnd . embedSnd . embedSnd . embedSnd

nestedRTupList :: [RNestedTup Int]
nestedRTupList = toNestedSnd <$> aList

-- * Test tree

runSorts :: (NFData a) => String -> (a -> a -> Ordering) -> [a] -> Benchmark
runSorts projString cmp list = do
  bgroup (projString <> " projection")
    [ bench "length" $ nf length list
    , bench "runs" $ nf (runs cmp) list
    , bench "calcAggregates" $ nf (calcAggregates cmp) list
    , bench "sortBy" $ nf (sortBy cmp) list
    , bench "mergeSortTopDown" $ nf (MSTD.sortBy cmp) list
    , bench "mergeSortTopDownWithVec" $ nf (MSTDV.sortBy cmp) list
    , bench "mergeSortBottomUp" $ nf (MSBU.sortBy cmp) list
    , bench "vectorSort" $ nf (VS.sortBy cmp) list
    ]

main :: IO ()
main = defaultMain
  [ runSorts "compare" compare aList
  , runSorts "comparing (snd . snd . snd . snd . snd . snd . snd)" (comparing nestedSnd) nestedRTupList
  ]
