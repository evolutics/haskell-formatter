module Evolutics.Code.Locations
       (Portioned, portion, Line(..), Column, formatMessage, mapPortions,
        successorLine, startLine, endLine, startColumn, createPosition,
        comparePortions, stringPortion)
       where
import qualified Data.Function as Function
import qualified Data.List as List
import qualified Evolutics.Code.Core as Core
import qualified Evolutics.Tools.Newlines as Newlines

class Portioned a where

        portion :: a -> Core.SrcSpan

data Line = Line Int
          deriving (Eq, Ord)

data Column = Column Int

instance Portioned Core.SrcSpanInfo where
        portion = Core.srcInfoSpan

formatMessage :: Core.SrcLoc -> String -> String
formatMessage position message
  = Core.prettyPrint position ++ separator ++ message
  where separator = ": "

mapPortions ::
            (Core.SrcSpan -> Core.SrcSpan) ->
              Core.SrcSpanInfo -> Core.SrcSpanInfo
mapPortions function nestedPortion
  = nestedPortion{Core.srcInfoSpan = mappedParent,
                  Core.srcInfoPoints = mappedChildren}
  where mappedParent = function parent
        parent = Core.srcInfoSpan nestedPortion
        mappedChildren = map function children
        children = Core.srcInfoPoints nestedPortion

successorLine :: Line -> Line
successorLine (Line line) = Line $ succ line

startLine :: Core.SrcSpan -> Line
startLine = Line . Core.startLine

endLine :: Core.SrcSpan -> Line
endLine = Line . Core.srcSpanEndLine

startColumn :: Core.SrcSpan -> Column
startColumn = Column . Core.startColumn

createPosition :: FilePath -> Line -> Column -> Core.SrcLoc
createPosition file (Line line) (Column column)
  = Core.SrcLoc{Core.srcFilename = file, Core.srcLine = line,
                Core.srcColumn = column}

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
