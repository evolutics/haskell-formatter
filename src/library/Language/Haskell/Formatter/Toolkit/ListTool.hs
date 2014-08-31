{-|
Description : List utilities
-}
module Language.Haskell.Formatter.Toolkit.ListTool
       (maybeLast, mergeConsecutiveElements, takeEvery, concatenateRuns,
        concatenateShiftedRuns)
       where
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Monoid as Monoid
import qualified Data.Word as Word

{-| The last element, or 'Nothing' if there is none.

    prop> maybeLast [] == Nothing
    prop> maybeLast (l ++ [e]) == Just e -}
maybeLast :: [a] -> Maybe a
maybeLast = Maybe.listToMaybe . reverse

{-| @mergeConsecutiveElements i l@ keeps only the first element of consecutive
    elements of @l@ satisfying the predicate @i@.

    >>> mergeConsecutiveElements Data.Char.isSpace " ab c  d\LF  e  "
    " ab c d\ne "
-}
mergeConsecutiveElements :: (a -> Bool) -> [a] -> [a]
mergeConsecutiveElements isMerged
  = snd . List.foldl' merge (False, [])
  where merge (isConsecutive, list) element = (isConsecutive', list')
          where isConsecutive' = isMerged element
                list' = Monoid.mappend list merged
                merged = if isConsecutive' && isConsecutive then [] else [element]

{-| @takeEvery p l@ takes every @p@th element of @l@ from the first one.

    >>> takeEvery 2 "apple"
    "ape"

    prop> takeEvery 1 l == l -}
takeEvery :: Word.Word -> [a] -> [a]
takeEvery _ [] = []
takeEvery period list@(first : _)
  = first : takeEvery period (drop (fromIntegral period) list)

{-| @concatenateRuns p l@ repeatedly concatenates @p@ lists of @l@.

    >>> concatenateRuns 2 ["a", "b", "c", "d", "e"]
    ["ab","cd","e"] -}
concatenateRuns :: Word.Word -> [[a]] -> [[a]]
concatenateRuns _ [] = []
concatenateRuns period lists
  = concat run : concatenateRuns period rest
  where (run, rest) = splitAt (fromIntegral period) lists

{-| @concatenateShiftedRuns p s l@ first takes @s@ lists of @l@, followed by
    repeatedly concatenating @p@ lists.

    >>> concatenateShiftedRuns 2 1 ["a", "b", "c", "d", "e"]
    ["a","bc","de"]

    prop> p == 0 || concatenateShiftedRuns p 0 l == concatenateRuns p l -}
concatenateShiftedRuns :: Word.Word -> Word.Word -> [[a]] -> [[a]]
concatenateShiftedRuns period shift lists
  = case shift of
        0 -> concatenateUnshifted lists
        _ -> concat shifted : concatenateUnshifted unshifted
          where (shifted, unshifted) = splitAt (fromIntegral shift) lists
  where concatenateUnshifted = concatenateRuns period
