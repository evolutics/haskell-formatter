module Evolutics.Tools.SourceLocations
       (formatMessage, comparePortions) where
import qualified Data.Function as Function
import qualified Data.List as List
import qualified Language.Haskell.Exts.Annotated as Exts

formatMessage :: Exts.SrcLoc -> String -> String
formatMessage location message
  = formatLocation location ++ separator ++ message
  where separator = ": "

formatLocation :: Exts.SrcLoc -> String
formatLocation (Exts.SrcLoc file line column)
  = List.intercalate separator $ file : map show [line, column]
  where separator = ":"

comparePortions :: Exts.SrcSpan -> Exts.SrcSpan -> Ordering
comparePortions left right
  = if Function.on (==) Exts.srcSpanFilename left right then
      compareIgnoringFile else EQ
  where compareIgnoringFile
          | Exts.srcSpanEnd left < Exts.srcSpanStart right = LT
          | Exts.srcSpanStart left > Exts.srcSpanEnd right = GT
          | otherwise = EQ
