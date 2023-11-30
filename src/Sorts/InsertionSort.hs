{-# LANGUAGE ScopedTypeVariables #-}

module Sorts.InsertionSort
  ( sortBy
  ) where

import qualified Data.List as L

sortBy :: forall a. (a -> a -> Ordering) -> [a] -> [a]
sortBy f = L.foldl' insert []
  where
    insert :: [a] -> a -> [a]
    insert [] y = [y]
    insert l@(x : xs) y
      | f x y == GT = y : l
      | otherwise = x : insert xs y
