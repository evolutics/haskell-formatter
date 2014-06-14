module Evolutics.Tools.Lists () where
import qualified Data.List as List
import qualified Evolutics.Tools.Functions as Functions

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
