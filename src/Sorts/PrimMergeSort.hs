{-# LANGUAGE ScopedTypeVariables #-}

module Sorts.PrimMergeSort
  ( sortBy
  ) where

import Control.Monad.ST
import Data.Primitive.Array

arrToList :: forall a. Array a -> [a]
arrToList arr = go 0
  where
    lenArr :: Int
    lenArr = sizeofArray arr

    go :: Int -> [a]
    go n
      | n == lenArr = []
      | otherwise = indexArray arr n : go (succ n)

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ l@[] = l
sortBy f xs = runST $ do
  aArray <- unsafeThawArray $ arrayFromListN len xs
  bArray <- cloneMutableArray aArray 0 len
  mergeSortBy f 0 len aArray bArray
  arrToList <$> unsafeFreezeArray bArray
  where
    len = length xs

-- start inclusive, end not inclusive
mergeSortBy :: (a -> a -> Ordering) -> Int -> Int -> MutableArray s a -> MutableArray s a -> ST s ()
mergeSortBy _ _ len _ _ | len <= 1 = pure ()
mergeSortBy f start len arr outArr = do
  let len' = len `div` 2
  mergeSortBy f start len' outArr arr
  let start' = start + len'
  let len'' = len - len'
  mergeSortBy f start' len'' outArr arr
  mergeBy f start start' (start' + len'') arr outArr

mergeBy :: forall s a. (a -> a -> Ordering) -> Int -> Int -> Int -> MutableArray s a -> MutableArray s a -> ST s ()
mergeBy f start1 start2 end arr outArr = do
  a <- readArray arr (pred start2)
  b <- readArray arr start2
  if f a b == LT
    -- Sublists already merged
    then copyMutableArray outArr start1 arr start1 (end - start1)
    else go start1 start1 start2
  where
    go :: Int -> Int -> Int -> ST s ()
    go iout i1 i2
      | iout == end = pure ()
      | i1 == start2 = copyMutableArray outArr iout arr i2 (end - i2)
      | i2 == end = copyMutableArray outArr iout arr i1 (start2 - i1)
      | otherwise = do
          a <- readArray arr i1
          b <- readArray arr i2
          if f a b /= GT
            then writeArray outArr iout a >> go (succ iout) (succ i1) i2
            else writeArray outArr iout b >> go (succ iout) i1 (succ i2)
