{-|
Description : Splitting lists on sublists
-}
module Language.Haskell.Formatter.Toolkit.Splitter (separate) where
import qualified Control.Applicative as Applicative
import qualified Data.List as List
import qualified Data.Monoid as Monoid
import qualified Language.Haskell.Formatter.Toolkit.ListTool as ListTool
import qualified Language.Haskell.Formatter.Toolkit.Visit as Visit

{-| Strategy to split a list on sublists. -}
data Splitter a = Splitter{delimiterPolicy :: DelimiterPolicy,
                           delimiterQueue :: [[a]]}
                deriving (Eq, Ord, Show)

{-| What to do with the delimiters? -}
data DelimiterPolicy = Drop
                     | Separate
                     | MergeLeft
                     | MergeRight
                     deriving (Eq, Ord, Show)

{-| @separate d l@ splits @l@ on the delimiters @d@, which are matched in the
    given order. The delimiters are not kept.

    >>> separate ["pineapple", "pine"] "0pineapple1"
    ["0","1"]
    >>> separate ["pine", "pineapple"] "0pineapple1"
    ["0","apple1"] -}
separate :: (Eq a) => [[a]] -> [a] -> [[a]]
separate = split . createSplitter Drop
  where createSplitter rawDelimiterPolicy rawDelimiterQueue
          = Splitter{delimiterPolicy = rawDelimiterPolicy,
                     delimiterQueue = rawDelimiterQueue}

{-| @split s l@ splits @l@ according to the strategy @s@. -}
split :: (Eq a) => Splitter a -> [a] -> [[a]]
split splitter list
  = case delimiterPolicy splitter of
        Drop -> ListTool.takeEvery period parts
        Separate -> parts
        MergeLeft -> ListTool.concatenateRuns period parts
        MergeRight -> ListTool.concatenateShiftedRuns period shift parts
          where shift = 1
  where period = 2
        parts = rawSplit (delimiterQueue splitter) list

{-| @rawSplit s l@ splits @l@ on the sublists @s@, keeping the separators.

    prop> odd . length $ separate ["apple", "pine"] l -}
rawSplit :: (Eq a) => [[a]] -> [a] -> [[a]]
rawSplit delimiters = move [] []
  where move parts left [] = Monoid.mappend parts [left]
        move parts left right@(first : rest)
          = case stripFirstPrefix delimiters right of
                Nothing -> move parts (Monoid.mappend left [first]) rest
                Just (delimiter, suffix) -> move
                                              (Monoid.mappend parts
                                                 [left, delimiter])
                                              []
                                              suffix

{-| @stripFirstPrefix p l@ returns the first element of @p@ which is a prefix of
    @l@ and the rest of @l@. It returns 'Nothing' if there is no such element.

    >>> stripFirstPrefix ["\LF", "\CR\LF", "\CR"] "\CR\LFpine"
    Just ("\r\n","pine")
    >>> stripFirstPrefix ["apple"] "pineapple"
    Nothing -}
stripFirstPrefix :: (Eq a) => [[a]] -> [a] -> Maybe ([a], [a])
stripFirstPrefix prefixes list = Visit.findJust strip prefixes
  where strip prefix = (,) prefix Applicative.<$> List.stripPrefix prefix list
