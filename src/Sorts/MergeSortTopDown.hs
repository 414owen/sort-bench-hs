module Sorts.MergeSortTopDown
  ( sortBy
  ) where

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy f lst = sortBy' f len lst
  where
    len = length lst

-- | Simple top-down mergeSort
sortBy' :: (a -> a -> Ordering) -> Int -> [a] -> [a]
sortBy' f len lst
  | len <= 1 = lst
  | otherwise =
      let 
        lenA = len `div` 2
        (l, r) = splitAt lenA lst
      in
      mergeBy f (sortBy' f lenA l) (sortBy' f (len - lenA) r)

mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy f l@(x : xs) r@(y : ys) = case f x y of
  GT -> y : mergeBy f l ys
  _ -> x : mergeBy f xs r
mergeBy _ [] xs = xs
mergeBy _ xs [] = xs
