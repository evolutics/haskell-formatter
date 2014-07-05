module Evolutics.Code.Helper
       (formatMessage, getStartPosition, getEndPosition, comparePortions)
       where
import Prelude hiding (getLine)
import qualified Evolutics.Code.Location as Location
import qualified Evolutics.Code.Source as Source

formatMessage :: Location.SrcLoc -> String -> String
formatMessage position message
  = Source.prettyPrint position ++ separator ++ message
  where separator = ": "

getStartPosition :: Location.SrcSpan -> Location.SrcLoc
getStartPosition
  = getPosition Location.getStartLine Location.getStartColumn

getPosition ::
            (Location.SrcSpan -> Location.Line) ->
              (Location.SrcSpan -> Location.Column) ->
                Location.SrcSpan -> Location.SrcLoc
getPosition getLine getColumn portion
  = Location.createPosition file line column
  where file = Location.fileName portion
        line = getLine portion
        column = getColumn portion

getEndPosition :: Location.SrcSpan -> Location.SrcLoc
getEndPosition
  = getPosition Location.getEndLine Location.getEndColumn

comparePortions :: Location.SrcSpan -> Location.SrcSpan -> Ordering
comparePortions left right
  | getEndPosition left < getStartPosition right = LT
  | getStartPosition left > getEndPosition right = GT
  | otherwise = EQ
