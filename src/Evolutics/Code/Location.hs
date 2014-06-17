module Evolutics.Code.Location
       (Line(..), successorLine, startLine, endLine) where
import qualified Language.Haskell.Exts.Annotated as Exts

data Line = Line Int
          deriving (Eq, Ord)

successorLine :: Line -> Line
successorLine (Line line) = Line $ succ line

startLine :: Exts.SrcSpan -> Line
startLine = Line . Exts.srcSpanStartLine

endLine :: Exts.SrcSpan -> Line
endLine = Line . Exts.srcSpanEndLine
