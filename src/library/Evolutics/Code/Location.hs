module Evolutics.Code.Location
       (SrcLoc.fileName, SrcLoc.SrcLoc, SrcLoc.SrcSpan,
        SrcLoc.SrcSpanInfo, base, plus, minus, Portioned, getPortion, Line,
        Column, getLine, getColumn, createPosition, SrcLoc.getPointLoc,
        getEndPosition, mapNestedPortion, stringPortion, getStartLine,
        getStartColumn, getEndLine, getEndColumn)
       where
import Prelude hiding (getLine)
import qualified Data.Function as Function
import qualified Language.Haskell.Exts.Annotated.Syntax as Syntax
import qualified Language.Haskell.Exts.Comments as Comments
import qualified Language.Haskell.Exts.SrcLoc as SrcLoc
import qualified Evolutics.Tools.Newlines as Newlines

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

getLine :: SrcLoc.SrcLoc -> Line
getLine = Line . SrcLoc.srcLine

getColumn :: SrcLoc.SrcLoc -> Column
getColumn = Column . SrcLoc.srcColumn

createPosition :: FilePath -> Line -> Column -> SrcLoc.SrcLoc
createPosition file (Line line) (Column column)
  = SrcLoc.SrcLoc{SrcLoc.srcFilename = file, SrcLoc.srcLine = line,
                  SrcLoc.srcColumn = column}

getEndPosition :: SrcLoc.SrcSpan -> SrcLoc.SrcLoc
getEndPosition portion = createPosition file line column
  where file = SrcLoc.fileName portion
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
        children' = map mapPortionFunction children
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
  where endPosition = createPosition file endLine endColumn
        file = SrcLoc.fileName startPosition
        endLine = sumPredecessor lineCount startLine
        sumPredecessor difference = pred . plus difference
        lineCount = length stringLines
        stringLines = Newlines.splitSeparatedLines string
        startLine = getStartLine startPosition
        endColumn = sumPredecessor lastLineLength lastLineStartColumn
        lastLineStartColumn = if hasSingleLine then startColumn else base
        hasSingleLine = lineCount == 1
        startColumn = getStartColumn startPosition
        lastLineLength = length $ last stringLines

getStartLine :: (SrcLoc.SrcInfo a) => a -> Line
getStartLine = getLine . SrcLoc.getPointLoc

getStartColumn :: (SrcLoc.SrcInfo a) => a -> Column
getStartColumn = getColumn . SrcLoc.getPointLoc

getEndLine :: SrcLoc.SrcSpan -> Line
getEndLine = getLine . getEndPosition

getEndColumn :: SrcLoc.SrcSpan -> Column
getEndColumn = getColumn . getEndPosition