{-# LANGUAGE NumericUnderscores #-}

module Main where

import Criterion.Main
import Data.List
import Data.Ord
import GHC.Exts (sortWith)

aList :: [Int]
aList = take 1_000 $ cycle $ concat $ permutations [0..20]

toFst :: (a -> b) -> a -> (b, a)
toFst f x = let y = f x in y `seq` (y, x)

compareFst :: Ord a => (a, b) -> (a, b) -> Ordering
compareFst = comparing fst

sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f
  = map snd
  . sortBy compareFst
  . map (toFst f)

main :: IO ()
main = defaultMain
  [ bench "sortOn" $ nf (sortOn id) aList
  , bench "sortWith" $ nf (sortWith id) aList
  , bench "sortOn'" $ nf (sortOn' id) aList
  ]
