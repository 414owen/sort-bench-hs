module Main
  ( main
  ) where

import Control.Monad
import Data.Ord              (comparing)
import Test.QuickCheck
import System.Exit

import qualified Data.List                         as L
import qualified Sorts.InsertionSort               as IS
import qualified Sorts.MergeSortBottomUp           as MSBU
import qualified Sorts.MergeSortBottomUpDList      as MSBUD
import qualified Sorts.MergeSortTopDownAlternating as MSTDA
import qualified Sorts.MergeSortTopDown            as MSTD
import qualified Sorts.MergeSortTopDownWithVec     as MSTDV
import qualified Sorts.PrimMergeSort               as PS
import qualified Sorts.QuickSort                   as QS
import qualified Sorts.VectorSort                  as VS

mergeSortTopDownProp :: [(Int, Int)] -> Property
mergeSortTopDownProp lst = L.sortBy (comparing fst) lst === MSTD.sortBy (comparing fst) lst

mergeSortTopDownAlternatingProp :: [(Int, Int)] -> Property
mergeSortTopDownAlternatingProp lst = L.sortBy (comparing fst) lst === MSTDA.sortBy (comparing fst) lst

mergeSortTopDownWithVecProp :: [(Int, Int)] -> Property
mergeSortTopDownWithVecProp lst = L.sortBy (comparing fst) lst === MSTDV.sortBy (comparing fst) lst

mergeSortBottomUpProp :: [(Int, Int)] -> Property
mergeSortBottomUpProp lst = L.sortBy (comparing fst) lst === MSBU.sortBy (comparing fst) lst

mergeSortBottomUpDListProp :: [(Int, Int)] -> Property
mergeSortBottomUpDListProp lst = L.sortBy (comparing fst) lst === MSBUD.sortBy (comparing fst) lst

vectorSortProp :: [(Int, Int)] -> Property
vectorSortProp lst = L.sortBy (comparing fst) lst === VS.sortBy (comparing fst) lst

quickSortProp :: [(Int, Int)] -> Property
quickSortProp lst = L.sortBy (comparing fst) lst === QS.sortBy (comparing fst) lst

primSortProp :: [(Int, Int)] -> Property
primSortProp lst = L.sortBy (comparing fst) lst === PS.sortBy (comparing fst) lst

insertionSortProp :: [(Int, Int)] -> Property
insertionSortProp lst = L.sortBy (comparing fst) lst === IS.sortBy (comparing fst) lst

check :: Testable prop => prop -> IO ()
check prop = do
  result <- quickCheckResult prop
  unless (isSuccess result) exitFailure

main :: IO ()
main = do
  check insertionSortProp
  check mergeSortTopDownProp
  check mergeSortTopDownAlternatingProp
  check mergeSortTopDownWithVecProp
  check mergeSortBottomUpProp
  check mergeSortBottomUpDListProp
  check quickSortProp
  check vectorSortProp
  check primSortProp
