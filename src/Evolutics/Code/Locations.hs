module Evolutics.Code.Locations
       (Portioned, portion, Line(..), formatMessage, successorLine,
        startLine, endLine, comparePortions, stringPortion)
       where
import qualified Data.Function as Function
import qualified Data.List as List
import qualified Evolutics.Code.Core as Core
import qualified Evolutics.Tools.Newlines as Newlines

class Portioned a where

        portion :: a -> Core.SrcSpan

data Line = Line Int
          deriving (Eq, Ord)

instance Portioned Core.SrcSpanInfo where
        portion = Core.srcInfoSpan

formatMessage :: Core.SrcLoc -> String -> String
formatMessage position message
  = Core.prettyPrint position ++ separator ++ message
  where separator = ": "

successorLine :: Line -> Line
successorLine (Line line) = Line $ succ line

startLine :: Core.SrcSpan -> Line
startLine = Line . Core.startLine

endLine :: Core.SrcSpan -> Line
endLine = Line . Core.srcSpanEndLine

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

stringPortion :: Core.SrcLoc -> String -> Core.SrcSpan
stringPortion startPosition string
  = Core.mkSrcSpan startPosition endPosition
  where endPosition
          = Core.SrcLoc{Core.srcFilename = file, Core.srcLine = endLine,
                        Core.srcColumn = endColumn}
        file = Core.fileName startPosition
        endLine = startLine + lineCount - 1
        startLine = Core.startLine startPosition
        lineCount = length lines
        lines = Newlines.splitSeparatedLines string
        endColumn = lastLineStartColumn + lastLineLength - 1
        lastLineStartColumn = if hasSingleLine then startColumn else 1
        hasSingleLine = lineCount == 1
        startColumn = Core.startColumn startPosition
        lastLineLength = length $ last lines
