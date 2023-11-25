{-# LANGUAGE NumericUnderscores  #-}

module Main
  ( main
  ) where

import Control.DeepSeq
import Criterion.Main
import Data.List
import Data.Tuple
import GHC.Exts (sortWith)

import OtherSorts

aList :: [Int]
aList = take 10_000 $ cycle $ concat $ permutations [0..20]

embedFst :: a -> (a, ())
embedFst a = (a, ())

-- * Test tree

sorts :: Ord b => [(String, (a -> b) -> [a] -> [a])]
sorts =
  [ ("sortOn", sortOn)
  , ("sortWith", sortWith)
  -- , ("sortBy . comparing", sortWith')
  -- , ("mergeSortWithVec", mergeSortWithVec)
  -- , ("mergeSortWithList", mergeSortWithList)
  ]

runSorts :: (NFData a, Ord b) => String -> (a -> b) -> [a] -> Benchmark
runSorts projString projection list
  = bgroup (projString <> " projection")
  $ flip fmap sorts
  $ \(sortStr, sortFn) -> bench sortStr $ nf (sortFn projection) list

main :: IO ()
main = defaultMain
  [ runSorts "id" id aList
  , runSorts "fst" fst $ embedFst <$> aList
  , runSorts "fst . fst"
      (fst . fst)
    $ (embedFst . embedFst) <$> aList
  , runSorts "fst . fst . fst"
      (fst . fst . fst)
    $ (embedFst . embedFst . embedFst) <$> aList
  , runSorts "fst . fst . fst . fst"
      (fst . fst . fst . fst)
    $ (embedFst . embedFst . embedFst . embedFst) <$> aList
  , runSorts "fst . fst . fst . fst . fst"
      (fst . fst . fst . fst . fst)
    $ (embedFst . embedFst . embedFst . embedFst . embedFst) <$> aList
  , runSorts "fst . fst . fst . fst . fst . fst"
      (fst . fst . fst . fst . fst . fst)
    $ (embedFst . embedFst . embedFst . embedFst . embedFst . embedFst) <$> aList
  , runSorts "fst . fst . fst . fst . fst . fst . fst"
      (fst . fst . fst . fst . fst . fst . fst)
    $ (embedFst . embedFst . embedFst . embedFst . embedFst . embedFst . embedFst) <$> aList
  ]
