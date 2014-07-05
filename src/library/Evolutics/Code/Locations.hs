module Evolutics.Code.Locations
       (formatMessage, getStartPosition, getEndPosition, comparePortions)
       where
import Prelude hiding (getLine)
import qualified Evolutics.Code.Source as Source

formatMessage :: Source.SrcLoc -> String -> String
formatMessage position message
  = Source.prettyPrint position ++ separator ++ message
  where separator = ": "

getStartPosition :: Source.SrcSpan -> Source.SrcLoc
getStartPosition
  = getPosition Source.getStartLine Source.getStartColumn

getPosition ::
            (Source.SrcSpan -> Source.Line) ->
              (Source.SrcSpan -> Source.Column) ->
                Source.SrcSpan -> Source.SrcLoc
getPosition getLine getColumn portion
  = Source.createPosition file line column
  where file = Source.fileName portion
        line = getLine portion
        column = getColumn portion

getEndPosition :: Source.SrcSpan -> Source.SrcLoc
getEndPosition = getPosition Source.getEndLine Source.getEndColumn

comparePortions :: Source.SrcSpan -> Source.SrcSpan -> Ordering
comparePortions left right
  | getEndPosition left < getStartPosition right = LT
  | getStartPosition left > getEndPosition right = GT
  | otherwise = EQ
