module Evolutics.Tools.Splitting (separate) where
import qualified Data.List as List
import qualified Data.List.Split.Internals as Internals
import qualified Evolutics.Tools.Functions as Functions
import qualified Evolutics.Tools.Lists as Lists

data Splitting a = Splitting{delimiters :: [[a]],
                             delimiterPolicy :: Internals.DelimPolicy}

separate :: (Eq a) => [[a]] -> [a] -> [[a]]
separate delimiterList
  = split
      Splitting{delimiters = delimiterList,
                delimiterPolicy = Internals.Drop}

split :: (Eq a) => Splitting a -> [a] -> [[a]]
split splitting list = processedDelimiters
  where processedDelimiters
          = case delimiterPolicy splitting of
                Internals.Drop -> Lists.takeEvery period raw
                Internals.Keep -> raw
                Internals.KeepLeft -> Lists.concatenateRuns period raw
                Internals.KeepRight -> Lists.concatenateShiftedRuns period shift
                                         raw
                  where shift = 1
        period = 2
        raw = rawSplit (delimiters splitting) list

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
  = Functions.findJust splitAgain . Lists.partitions
  where splitAgain (left, right)
          = fmap (prepend left) $ stripFirstPrefix separators right
        prepend left (middle, right) = (left, middle, right)

stripFirstPrefix :: (Eq a) => [[a]] -> [a] -> Maybe ([a], [a])
stripFirstPrefix prefixes list = Functions.findJust strip prefixes
  where strip prefix
          = fmap ((,) prefix) $ List.stripPrefix prefix list
