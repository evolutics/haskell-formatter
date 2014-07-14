module Evolutics.Tools.Lists
       (takeEvery, concatenateRuns, concatenateShiftedRuns) where

takeEvery :: Int -> [a] -> [a]
takeEvery _ [] = []
takeEvery period list@(first : _)
  = first : takeEvery period (drop period list)

concatenateRuns :: Int -> [[a]] -> [[a]]
concatenateRuns _ [] = []
concatenateRuns period lists
  = concat run : concatenateRuns period rest
  where (run, rest) = splitAt period lists

concatenateShiftedRuns :: Int -> Int -> [[a]] -> [[a]]
concatenateShiftedRuns period shift lists
  = case shift of
        0 -> concatenateUnshifted lists
        _ -> concat shifted : concatenateUnshifted unshifted
          where (shifted, unshifted) = splitAt shift lists
  where concatenateUnshifted = concatenateRuns period
