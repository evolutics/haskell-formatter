module Language.Haskell.Formatter.Code.Location
       (SrcLoc.SrcLoc, SrcLoc.SrcSpan, SrcLoc.SrcSpanInfo, base, plus,
        minus, Portioned, getPortion, Line, Column, streamName, getLine,
        getColumn, createPosition, SrcLoc.getPointLoc, getEndPosition,
        mapNestedPortion, stringPortion, getStartLine, getStartColumn,
        getEndLine, getEndColumn)
       where
import Prelude hiding (getLine)
import qualified Data.Function as Function
import qualified Language.Haskell.Exts.Annotated.Syntax as Syntax
import qualified Language.Haskell.Exts.Comments as Comments
import qualified Language.Haskell.Exts.SrcLoc as SrcLoc
import qualified Language.Haskell.Formatter.Toolkit.ListTool
       as ListTool
import qualified Language.Haskell.Formatter.Toolkit.Newline
       as Newline
import qualified Language.Haskell.Formatter.Toolkit.StreamName
       as StreamName

class (Enum a) => Natural a where

        base :: a

        plus :: (Integral b) => b -> a -> a
        plus difference natural
          = toEnum $ fromIntegral difference + fromEnum natural

        minus :: (Num b) => a -> a -> b
        minus minuend = fromIntegral . Function.on (-) fromEnum minuend

class Portioned a where

        getPortion :: a -> SrcLoc.SrcSpan

data Line = Line Int
          deriving (Eq, Ord, Show)

data Column = Column Int
            deriving (Eq, Ord, Show)

instance Enum Line where
        toEnum = Line
        fromEnum (Line line) = line

instance Enum Column where
        toEnum = Column
        fromEnum (Column column) = column

instance Natural Line where
        base = Line 1

instance Natural Column where
        base = Column 1

instance Portioned SrcLoc.SrcSpanInfo where
        getPortion = SrcLoc.srcInfoSpan

instance (Portioned a) => Portioned (Syntax.Module a) where
        getPortion = getPortion . Syntax.ann

instance Portioned Comments.Comment where
        getPortion (Comments.Comment _ commentPortion _) = commentPortion

streamName :: (SrcLoc.SrcInfo a) => a -> StreamName.StreamName
streamName = StreamName.createStreamName . SrcLoc.fileName

getLine :: SrcLoc.SrcLoc -> Line
getLine = Line . SrcLoc.srcLine

getColumn :: SrcLoc.SrcLoc -> Column
getColumn = Column . SrcLoc.srcColumn

createPosition ::
               StreamName.StreamName -> Line -> Column -> SrcLoc.SrcLoc
createPosition stream (Line line) (Column column)
  = SrcLoc.SrcLoc{SrcLoc.srcFilename = show stream,
                  SrcLoc.srcLine = line, SrcLoc.srcColumn = column}

getEndPosition :: SrcLoc.SrcSpan -> SrcLoc.SrcLoc
getEndPosition portion = createPosition stream line column
  where stream = streamName portion
        line = Line $ SrcLoc.srcSpanEndLine portion
        column = Column $ SrcLoc.srcSpanEndColumn portion

mapNestedPortion ::
                 (Line -> Line) -> SrcLoc.SrcSpanInfo -> SrcLoc.SrcSpanInfo
mapNestedPortion function nestedPortion
  = nestedPortion{SrcLoc.srcInfoSpan = parent',
                  SrcLoc.srcInfoPoints = children'}
  where parent' = mapPortionFunction parent
        mapPortionFunction = mapPortion function
        parent = SrcLoc.srcInfoSpan nestedPortion
        children' = fmap mapPortionFunction children
        children = SrcLoc.srcInfoPoints nestedPortion

mapPortion :: (Line -> Line) -> SrcLoc.SrcSpan -> SrcLoc.SrcSpan
mapPortion function portion
  = portion{SrcLoc.srcSpanStartLine = start,
            SrcLoc.srcSpanEndLine = end}
  where Line start = function $ getStartLine portion
        Line end = function $ getEndLine portion

stringPortion :: SrcLoc.SrcLoc -> String -> SrcLoc.SrcSpan
stringPortion startPosition string
  = SrcLoc.mkSrcSpan startPosition endPosition
  where endPosition = createPosition stream endLine endColumn
        stream = streamName startPosition
        endLine = sumPredecessor lineCount startLine
        sumPredecessor difference = pred . plus difference
        lineCount = length stringLines
        stringLines = Newline.splitSeparatedLines string
        startLine = getStartLine startPosition
        endColumn = sumPredecessor lastLineLength lastLineStartColumn
        lastLineLength = maybe 0 length $ ListTool.maybeLast stringLines
        lastLineStartColumn = if hasSingleLine then startColumn else base
        hasSingleLine = lineCount == 1
        startColumn = getStartColumn startPosition

getStartLine :: (SrcLoc.SrcInfo a) => a -> Line
getStartLine = getLine . SrcLoc.getPointLoc

getStartColumn :: (SrcLoc.SrcInfo a) => a -> Column
getStartColumn = getColumn . SrcLoc.getPointLoc

getEndLine :: SrcLoc.SrcSpan -> Line
getEndLine = getLine . getEndPosition

getEndColumn :: SrcLoc.SrcSpan -> Column
getEndColumn = getColumn . getEndPosition
