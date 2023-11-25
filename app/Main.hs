{-# LANGUAGE NumericUnderscores  #-}

module Main where

import Control.DeepSeq
import Criterion.Main
import Data.List
import Data.Tuple
import GHC.Exts (sortWith)

import OtherSorts

aList :: [Int]
aList = take 10_000 $ cycle $ concat $ permutations [0..20]

tupList :: [(Int, Int)]
tupList = zip aList [0..]

-- * Large right-tuples
-- maybe cache lines play a role?

type SevenTup a = ((), (), (), (), (), (), a)

toSeventh :: a -> SevenTup a
toSeventh a = ((), (), (), (), (), (), a)

seventh :: SevenTup a -> a
seventh (_, _, _, _, _, _, a) = a

sevenTupList :: [SevenTup Int]
sevenTupList = toSeventh <$> aList


-- * Nested tuples

type LNestedTup a = (((((((a, ()), ()), ()), ()), ()), ()), ())

nestedFst :: LNestedTup a -> a
nestedFst = fst . fst . fst . fst . fst . fst . fst

toNestedFst :: a -> LNestedTup a
toNestedFst a = (((((((a, ()), ()), ()), ()), ()), ()), ())

nestedLTupList :: [LNestedTup Int]
nestedLTupList = toNestedFst <$> aList

type RNestedTup a = ((), ((), ((), ((), ((), ((), ((), a)))))))

nestedSnd :: RNestedTup a -> a
nestedSnd = snd . snd . snd . snd . snd . snd . snd

toNestedSnd :: a -> RNestedTup a
toNestedSnd a = ((), ((), ((), ((), ((), ((), ((), a)))))))

nestedRTupList :: [RNestedTup Int]
nestedRTupList = toNestedSnd <$> aList

-- * Test tree

sorts :: Ord b => [(String, (a -> b) -> [a] -> [a])]
sorts =
  [ ("sortOn", sortOn)
  , ("sortWith", sortWith)
  , ("sortBy . comparing", sortWith')
  , ("mergeWortWith", mergeSortWith)
  ]

runSorts :: (NFData a, Ord b) => String -> (a -> b) -> [a] -> Benchmark
runSorts projString projection list
  = bgroup (projString <> " projection")
  $ flip fmap sorts
  $ \(sortStr, sortFn) -> bench sortStr $ nf (sortFn projection) list

main :: IO ()
main = defaultMain
  [ runSorts "id" id aList
  , runSorts "fst" fst tupList
  , runSorts "snd" snd $ swap <$> tupList
  , runSorts "seventh" seventh sevenTupList
  , runSorts "(fst . fst . fst . fst . fst . fst . fst)" nestedFst nestedLTupList
  , runSorts "(snd . snd . snd . snd . snd . snd . snd)" nestedSnd nestedRTupList
  ]
