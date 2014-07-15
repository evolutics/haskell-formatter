module Evolutics.Tools.Splitting (separate) where
import qualified Data.List as List
import qualified Data.Monoid as Monoid
import qualified Evolutics.Tools.Functions as Functions
import qualified Evolutics.Tools.Lists as Lists

data Splitting a = Splitting{delimiters :: [[a]],
                             delimiterPolicy :: DelimiterPolicy}
                 deriving (Eq, Ord, Show)

data DelimiterPolicy = Drop
                     | Separate
                     | MergeLeft
                     | MergeRight
                     deriving (Eq, Ord, Show)

separate :: (Eq a) => [[a]] -> [a] -> [[a]]
separate delimiterList
  = split
      Splitting{delimiters = delimiterList, delimiterPolicy = Drop}

split :: (Eq a) => Splitting a -> [a] -> [[a]]
split splitting list = processedDelimiters
  where processedDelimiters
          = case delimiterPolicy splitting of
                Drop -> Lists.takeEvery period raw
                Separate -> raw
                MergeLeft -> Lists.concatenateRuns period raw
                MergeRight -> Lists.concatenateShiftedRuns period shift raw
                  where shift = 1
        period = 2
        raw = rawSplit (delimiters splitting) list

rawSplit :: (Eq a) => [[a]] -> [a] -> [[a]]
rawSplit separators = move [] []
  where move parts left [] = Monoid.mappend parts [left]
        move parts left right@(element : rest)
          = case stripFirstPrefix separators right of
                Nothing -> move parts (Monoid.mappend left [element]) rest
                Just (separator, suffix) -> move
                                              (Monoid.mappend parts [left, separator])
                                              []
                                              suffix

stripFirstPrefix :: (Eq a) => [[a]] -> [a] -> Maybe ([a], [a])
stripFirstPrefix prefixes list = Functions.findJust strip prefixes
  where strip prefix
          = fmap ((,) prefix) $ List.stripPrefix prefix list
