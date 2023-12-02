{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ViewPatterns        #-}

module Main
  ( main
  ) where

import Control.Monad.Random.Strict
import Control.DeepSeq
import Criterion.Main
import Data.Ord (comparing)
import Data.List

import OtherSorts

import qualified Sorts.InsertionSort               as IS
import qualified Sorts.MergeSortTopDown            as MSTD
import qualified Sorts.MergeSortTopDownAlternating as MSTDA
import qualified Sorts.MergeSortBottomUp           as MSBU
import qualified Sorts.MergeSortBottomUpDList      as MSBUD
import qualified Sorts.MergeSortTopDownWithVec     as MSTDV
import qualified Sorts.PrimMergeSort               as PS
import qualified Sorts.QuickSort                   as QS
import qualified Sorts.VectorSort                  as VS

-- * Nested tuples

type RNestedTup a = ((), ((), ((), ((), ((), ((), ((), a)))))))

nestedSnd :: RNestedTup a -> a
nestedSnd = snd . snd . snd . snd . snd . snd . snd

embedSnd :: a -> ((), a)
embedSnd a = ((), a)

toNestedSnd :: a -> RNestedTup a
toNestedSnd = embedSnd . embedSnd . embedSnd . embedSnd . embedSnd . embedSnd . embedSnd

randomSeed :: StdGen
randomSeed = mkStdGen 42

-- Lists of ints with different patterns,
-- some with runs, some with repeating elements
-- some purely random...
listVariants :: [(String, [Int])]
listVariants =
  -- Lots of runs, Lots of same elements
  [
  --   ("cycled permutations", cycle $ concat $ permutations [0..20])
    ("already sorted list", [0..])
  -- ("descending sorted list", [0, (-1) ..])
  -- ("completely random list", randoms randomSeed)
  ]

-- * Test tree

runSorts :: (NFData a) => String -> (a -> a -> Ordering) -> [a] -> Benchmark
runSorts projString cmp list = do
  bgroup (projString <> " projection")
    [
    --   bench "length" $ nf length list
    -- , bench "runs" $ nf (runs cmp) list
    -- , bench "calcAggregates" $ nf (calcAggregates cmp) list
    --   bench "sortBy" $ nf (sortBy cmp) list
    -- , bench "insertionSort" $ nf (IS.sortBy cmp) list
      bench "mergeSortTopDown" $ nf (MSTD.sortBy cmp) list
    -- , bench "mergeSortTopDownAlternating" $ nf (MSTDA.sortBy cmp) list
    -- , bench "mergeSortTopDownWithVec" $ nf (MSTDV.sortBy cmp) list
    , bench "mergeSortBottomUp" $ nf (MSBU.sortBy cmp) list
    , bench "mergeSortBottomUpDLIst" $ nf (MSBUD.sortBy cmp) list
    -- , bench "vectorSort" $ nf (VS.sortBy cmp) list
    -- , bench "quickSort" $ nf (QS.sortBy cmp) list
    -- , bench "primSort" $ nf (PS.sortBy cmp) list
    ]

listLength :: Int
listLength = 100_000

main :: IO ()
main = defaultMain $ flip fmap listVariants $ \(variantName, take listLength -> lst) ->
  bgroup variantName
  [ runSorts "compare" compare lst
  , runSorts "comparing (snd . snd . snd . snd . snd . snd . snd)" (comparing nestedSnd) $ toNestedSnd <$> lst
  ]
