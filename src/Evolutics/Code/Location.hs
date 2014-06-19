module Evolutics.Code.Location
       (Line(..), successorLine, startLine, endLine) where
import qualified Evolutics.Code.Core as Core

data Line = Line Int
          deriving (Eq, Ord)

successorLine :: Line -> Line
successorLine (Line line) = Line $ succ line

startLine :: Core.SrcSpan -> Line
startLine = Line . Core.srcSpanStartLine

endLine :: Core.SrcSpan -> Line
endLine = Line . Core.srcSpanEndLine
