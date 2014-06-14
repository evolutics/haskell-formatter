module Evolutics.Tools.Lists () where
import qualified Data.List as List
import qualified Data.List.Split.Internals as Internals
import qualified Evolutics.Tools.Functions as Functions

data Splitting a = Splitting{delimiters :: [[a]],
                             delimiterPolicy :: Internals.DelimPolicy}

split :: (Eq a) => Splitting a -> [a] -> [[a]]
split splitting list = processedDelimiters
  where processedDelimiters
          = case delimiterPolicy splitting of
                Internals.Drop -> takeEvery period raw
                Internals.Keep -> raw
                Internals.KeepLeft -> concatenatePairs raw
                Internals.KeepRight -> case raw of
                                           (first : rest) -> first : concatenatePairs rest
        period = 2
        raw = rawSplit (delimiters splitting) list
        concatenatePairs = concatenateNeighbors period

takeEvery :: Int -> [a] -> [a]
takeEvery _ [] = []
takeEvery period list@(first : _)
  = first : takeEvery period (drop period list)

rawSplit :: (Eq a) => [[a]] -> [a] -> [[a]]
rawSplit separators = Functions.untilRight move . (,) []
  where move (splitLists, restList)
          = case findFirstSeparator separators restList of
                Nothing -> Right $ splitLists ++ [restList]
                Just (left, separator, right) -> Left
                                                   (splitLists ++ [left, separator], right)

findFirstSeparator ::
                     (Eq a) => [[a]] -> [a] -> Maybe ([a], [a], [a])
findFirstSeparator separators
  = Functions.findJust split . partitions
  where split (left, right)
          = fmap (prepend left) $ stripFirstPrefix separators right
        prepend left (middle, right) = (left, middle, right)

partitions :: [a] -> [([a], [a])]
partitions list = Functions.iterateUntilNothing move ([], list)
  where move (_, []) = Nothing
        move (left, middle : right) = Just (left ++ [middle], right)

stripFirstPrefix :: (Eq a) => [[a]] -> [a] -> Maybe ([a], [a])
stripFirstPrefix prefixes list = Functions.findJust strip prefixes
  where strip prefix
          = fmap ((,) prefix) $ List.stripPrefix prefix list

concatenateNeighbors :: Int -> [[a]] -> [[a]]
concatenateNeighbors _ [] = []
concatenateNeighbors period lists
  = concat list : concatenateNeighbors period rest
  where (list, rest) = splitAt period lists
