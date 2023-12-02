
{-# LANGUAGE ScopedTypeVariables #-}

module Sorts.MergeSortBottomUpBalanced
  ( sortBy
  ) where

-- This is pretty much identical to the `sortBy` actually used
-- in `base`, except that it reverses runs, instead of using a
-- dlist.

-- It also tries to balance the merge by keeping track of the
-- length of the list being merged, so as not to end up with
-- a situation like this: `mergePairs [[1..1000], []]`.

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
mergeSort f xs = mergeSort' f (length xs) (mergePairs f xs)

mergeSort' :: (a -> a -> Ordering) -> Int -> [[a]] -> [a]
mergeSort' _ _ [] = []
mergeSort' _ _ [l] = l
mergeSort' f n l@(x : xs)
  | even n = mergeSort' f (n `div` 2) (mergePairs f l)
  | otherwise = case mergePairs f xs of
    (y : ys) -> mergeSort' f (n `div` 2) (mergeBy f x y : ys)
    [] -> [] -- impossible

mergePairs :: (a -> a -> Ordering) -> [[a]] -> [[a]]
mergePairs f (x1 : x2 : xs) = mergeBy f x1 x2 : mergePairs f xs
mergePairs _ l = l

mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy f l@(x : xs) r@(y : ys) = case f x y of
  GT -> y : mergeBy f l ys
  _ -> x : mergeBy f xs r
mergeBy _ [] xs = xs
mergeBy _ xs [] = xs
