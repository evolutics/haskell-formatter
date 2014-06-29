module Evolutics.Code.Locations
       (Portioned, portion, Line(..), Column, formatMessage, mapPortions,
        successorLine, startLine, endLine, firstColumn, startColumn,
        createPosition, startPosition, comparePortions, stringPortion)
       where
import qualified Data.Function as Function
import qualified Evolutics.Code.Source as Source
import qualified Evolutics.Tools.Newlines as Newlines

class Portioned a where

        portion :: a -> Source.SrcSpan

data Line = Line Int
          deriving (Eq, Ord)

data Column = Column Int

instance Portioned Source.SrcSpanInfo where
        portion = Source.srcInfoSpan

instance (Portioned a) => Portioned (Source.Module a) where
        portion = portion . Source.ann

formatMessage :: Source.SrcLoc -> String -> String
formatMessage position message
  = Source.prettyPrint position ++ separator ++ message
  where separator = ": "

mapPortions ::
            (Source.SrcSpan -> Source.SrcSpan) ->
              Source.SrcSpanInfo -> Source.SrcSpanInfo
mapPortions function nestedPortion
  = nestedPortion{Source.srcInfoSpan = parent',
                  Source.srcInfoPoints = children'}
  where parent' = function parent
        parent = Source.srcInfoSpan nestedPortion
        children' = map function children
        children = Source.srcInfoPoints nestedPortion

successorLine :: Line -> Line
successorLine (Line line) = Line $ succ line

startLine :: (Source.SrcInfo a) => a -> Line
startLine = Line . Source.startLine

endLine :: Source.SrcSpan -> Line
endLine = Line . Source.srcSpanEndLine

firstColumn :: Column
firstColumn = Column 1

startColumn :: (Source.SrcInfo a) => a -> Column
startColumn = Column . Source.startColumn

createPosition :: FilePath -> Line -> Column -> Source.SrcLoc
createPosition file (Line line) (Column column)
  = Source.SrcLoc{Source.srcFilename = file, Source.srcLine = line,
                  Source.srcColumn = column}

startPosition :: (Portioned a) => a -> Source.SrcLoc
startPosition = Source.getPointLoc . portion

comparePortions :: (Portioned a, Portioned b) => a -> b -> Ordering
comparePortions leftPortioned rightPortioned
  = if Function.on (==) Source.fileName left right then
      compareIgnoringFile else EQ
  where left = portion leftPortioned
        right = portion rightPortioned
        compareIgnoringFile
          | Source.srcSpanEnd left < Source.srcSpanStart right = LT
          | Source.srcSpanStart left > Source.srcSpanEnd right = GT
          | otherwise = EQ

stringPortion :: Source.SrcLoc -> String -> Source.SrcSpan
stringPortion startPosition string
  = Source.mkSrcSpan startPosition endPosition
  where endPosition
          = startPosition{Source.srcLine = endLine,
                          Source.srcColumn = endColumn}
        endLine = startLine + lineCount - 1
        startLine = Source.startLine startPosition
        lineCount = length lines
        lines = Newlines.splitSeparatedLines string
        endColumn = lastLineStartColumn + lastLineLength - 1
        lastLineStartColumn = if hasSingleLine then startColumn else 1
        hasSingleLine = lineCount == 1
        startColumn = Source.startColumn startPosition
        lastLineLength = length $ last lines
