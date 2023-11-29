module Sorts.MergeSortTopDown
  ( sortBy
  ) where

-- | Simple top-down mergeSort
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy _ [a] = [a]
sortBy f list
  = let (l, r) = splitList list in
  mergeBy f (sortBy f l) (sortBy f r)

splitList :: [a] -> ([a], [a])
splitList = splitList' ([], [])

splitList' :: ([a], [a]) -> [a] -> ([a], [a])
splitList' (outX, outY) inputs = case inputs of
  (x1 : x2 : xs) -> splitList' (x1 : outX, x2 : outY) xs
  [x] -> (x : outX, outY)
  [] -> (outX, outY)

mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy f l@(x : xs) r@(y : ys) = case f x y of
  LT -> x : mergeBy f xs r
  GT -> y : mergeBy f l ys
  EQ -> x : y : mergeBy f xs ys
mergeBy _ _ _ = []
