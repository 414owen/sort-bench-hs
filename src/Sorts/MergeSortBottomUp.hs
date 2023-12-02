{-# LANGUAGE ScopedTypeVariables #-}

module Sorts.MergeSortBottomUp
  ( sortBy
  ) where

-- This is pretty much identical to the `sortBy` actually used
-- in `base`, except that it reverses runs, instead of using a
-- dlist.

sortBy :: forall a. (a -> a -> Ordering) -> [a] -> [a]
sortBy f = mergeSort f . runs f

runs :: forall a. (a -> a -> Ordering) -> [a] -> [[a]]
runs f (x1 : x2 : xs) = case f x1 x2 of
  GT -> let (l, rest) = descRun x2 [x2, x1] xs in l : runs f rest
  _ -> let (l, rest) = ascRun x2 [] xs in (x1 : x2 : reverse l) : runs f rest

  where
    ascRun :: a -> [a] -> [a] -> ([a], [a])
    ascRun lastEl acc (y1 : ys)
      | f lastEl y1 /= GT = ascRun y1 (y1 : acc) ys
    ascRun _ acc rest = (acc, rest)

    descRun :: a -> [a] -> [a] -> ([a], [a])
    descRun lastEl acc (y1 : ys)
      | f lastEl y1 == GT = descRun y1 (y1 : acc) ys
    descRun _ acc rest = (acc, rest)

runs _ l = [l]


mergeSort :: (a -> a -> Ordering) -> [[a]] -> [a]
mergeSort _ [] = []
mergeSort _ [xs] = xs
mergeSort f xs = mergeSort f (mergePairs f xs)

mergePairs :: (a -> a -> Ordering) -> [[a]] -> [[a]]
mergePairs f (x1 : x2 : xs) = mergeBy f x1 x2 : mergePairs f xs
mergePairs _ l = l

mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy f l@(x : xs) r@(y : ys) = case f x y of
  GT -> y : mergeBy f l ys
  _ -> x : mergeBy f xs r
mergeBy _ [] xs = xs
mergeBy _ xs [] = xs

