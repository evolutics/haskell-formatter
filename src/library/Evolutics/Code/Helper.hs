module Evolutics.Code.Helper (formatMessage, portionDisplacement)
       where
import qualified Data.Function as Function
import qualified Data.Ratio as Ratio
import qualified Evolutics.Code.Location as Location
import qualified Evolutics.Code.Source as Source

formatMessage :: Location.SrcLoc -> String -> String
formatMessage position message
  = Source.prettyPrint position ++ separator ++ message
  where separator = ": "

portionDisplacement ::
                    Location.SrcSpan -> Location.SrcSpan -> Ordering
portionDisplacement = Function.on compare portionCenter

portionCenter :: Location.SrcSpan -> Location.SrcLoc
portionCenter portion = Location.createPosition file line column
  where file = Location.fileName portion
        line = center startLine endLine
        center start end = Location.plus half start
          where half = round $ difference Ratio.% 2 :: Integer
                difference = Location.minus end start :: Integer
        startLine = Location.getStartLine portion
        endLine = Location.getEndLine portion
        column = center startColumn endColumn
        startColumn = Location.getStartColumn portion
        endColumn = Location.getEndColumn portion
