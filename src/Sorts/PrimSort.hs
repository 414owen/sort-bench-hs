{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Sorts.PrimSort
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

sortBy :: forall a. (a -> a -> Ordering) -> [a] -> [a]
sortBy f l@[] = l
sortBy f xs = runST $ do
  arr <- unsafeThawArray $ arrayFromListN len xs
  mergeSort 0 len arr
  arrToList <$> unsafeFreezeArray arr
  where
    len = length xs

-- start inclusive, end not inclusive
mergeSort :: Int -> Int -> MutableArray s a -> ST s ()
mergeSort start len _ | len <= 1 = pure ()
mergeSort start len arr = do
  let len' = len `div` 2
  mergeSort start len' arr
  let start' = start + len'
  let len'' = len - len'
  mergeSort start' len'' arr
  merge start start' (start' + len'') arr

merge :: Int -> Int -> Int -> Int -> MutableArray s a -> ST s ()
merge start1 start2 end arr
  | start2 == end1 = pure ()
  | start1 == start2 = pure ()
  | otherwise = do
      a <- readArray arr start1
      b <- readArray arr start2
      pure ()

