{-# LANGUAGE NumericUnderscores  #-}

module Main
  ( main
  ) where

import Criterion.Main
import Data.List      (foldl')

testList :: [Int]
testList = take 1_000_000 [1..]

accumulateDlist :: [a] -> [a]
accumulateDlist = ($ []) . foldl' (\acc el -> acc . (el:)) id

accumulateRev :: [a] -> [a]
accumulateRev = reverse . foldl' (flip (:)) []

main :: IO ()
main = defaultMain
  [ env (pure testList) $ \lst -> bgroup "Appending to lists"
    [ bench "reverse . conses" $ nf accumulateRev lst
    , bench "dlist" $ nf accumulateDlist lst
    ]
  ]
