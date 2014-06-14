module Evolutics.Tools.Lists
       (takeEvery, concatenateNeighbors, partitions) where
import qualified Evolutics.Tools.Functions as Functions

takeEvery :: Int -> [a] -> [a]
takeEvery _ [] = []
takeEvery period list@(first : _)
  = first : takeEvery period (drop period list)

concatenateNeighbors :: Int -> [[a]] -> [[a]]
concatenateNeighbors _ [] = []
concatenateNeighbors period lists
  = concat list : concatenateNeighbors period rest
  where (list, rest) = splitAt period lists

partitions :: [a] -> [([a], [a])]
partitions list = Functions.iterateUntilNothing move ([], list)
  where move (_, []) = Nothing
        move (left, middle : right) = Just (left ++ [middle], right)
