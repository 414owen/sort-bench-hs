{-# LANGUAGE ScopedTypeVariables #-}

module Sorts.QuickSort
  ( sortBy
  ) where

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ l@[] = l
sortBy _ l@[_] = l
sortBy f (x : xs) = let (l, r) = partition' ((== GT) . f x) xs in
  sortBy f l <> [x] <> sortBy f r

partition' :: forall a. (a -> Bool) -> [a] -> ([a], [a])
partition' f = bireverse . go ([], [])
  where
    go :: ([a], [a]) -> [a] -> ([a], [a])
    go acc [] = acc
    go (xs, ys) (a : as)
      | f a       = go (a : xs, ys) as
      | otherwise = go (xs, a : ys) as

bireverse :: ([a], [b]) -> ([a], [b])
bireverse (xs, ys) = (reverse xs, reverse ys)
