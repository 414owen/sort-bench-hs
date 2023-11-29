module Sorts.VectorSort
  ( sortBy
  ) where

import Control.Monad.ST (runST)

import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Merge as VT

-- | Simple top-down mergeSort
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy f lst = runST $ do
  v <- V.thaw $ V.fromList lst
  VT.sortBy f v
  V.toList <$> V.freeze v
