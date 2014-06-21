module Evolutics.Tools.SourceLocations
       (Portioned, portion, formatMessage, comparePortions) where
import qualified Data.Function as Function
import qualified Data.List as List
import qualified Evolutics.Code.Core as Core

class Portioned a where

        portion :: a -> Core.SrcSpan

instance Portioned Core.SrcSpanInfo where
        portion = Core.srcInfoSpan

formatMessage :: Core.SrcLoc -> String -> String
formatMessage position message
  = formatPosition position ++ separator ++ message
  where separator = ": "

formatPosition :: Core.SrcLoc -> String
formatPosition (Core.SrcLoc file line column)
  = List.intercalate separator $ file : map show [line, column]
  where separator = ":"

comparePortions :: (Portioned a, Portioned b) => a -> b -> Ordering
comparePortions leftPortioned rightPortioned
  = if Function.on (==) Core.fileName left right then
      compareIgnoringFile else EQ
  where left = portion leftPortioned
        right = portion rightPortioned
        compareIgnoringFile
          | Core.srcSpanEnd left < Core.srcSpanStart right = LT
          | Core.srcSpanStart left > Core.srcSpanEnd right = GT
          | otherwise = EQ
