module Evolutics.Code.Source
       (Annotated.parseFileContents,
        Annotated.parseFileContentsWithComments, Comments.Comment,
        ExactPrint.exactPrint, Parser.defaultParseMode,
        Parser.fromParseResult, Parser.parseFilename,
        Parser.ParseResult(..), Pretty.prettyPrint, SrcLoc.fileName,
        SrcLoc.SrcLoc, SrcLoc.SrcSpan, SrcLoc.SrcSpanInfo, (Syntax.=~=),
        Syntax.amap, Syntax.Module, zero, add, Portioned, getPortion, Line,
        Column, getStartLine, getStartColumn, getEndLine, getEndColumn,
        createPosition, mapNestedPortion, stringPortion, createComment,
        commentCore)
       where
import qualified Language.Haskell.Exts.Annotated as Annotated
import qualified Language.Haskell.Exts.Annotated.ExactPrint
       as ExactPrint
import qualified Language.Haskell.Exts.Annotated.Syntax as Syntax
import qualified Language.Haskell.Exts.Comments as Comments
import qualified Language.Haskell.Exts.Parser as Parser
import qualified Language.Haskell.Exts.Pretty as Pretty
import qualified Language.Haskell.Exts.SrcLoc as SrcLoc
import qualified Evolutics.Code.Comment as Comment
import qualified Evolutics.Tools.Newlines as Newlines

class (Enum a) => Natural a where

        zero :: a

        add :: (Integral b) => b -> a -> a
        add difference ordered
          = toEnum $ fromIntegral difference + fromEnum ordered

class Portioned a where

        getPortion :: a -> SrcLoc.SrcSpan

data Line = Line Int
          deriving (Eq, Ord, Show, Read)

data Column = Column Int
            deriving (Eq, Ord, Show, Read)

instance Enum Line where
        toEnum = Line
        fromEnum (Line line) = line

instance Enum Column where
        toEnum = Column
        fromEnum (Column column) = column

instance Natural Line where
        zero = Line 1

instance Natural Column where
        zero = Column 1

instance Portioned SrcLoc.SrcSpanInfo where
        getPortion = SrcLoc.srcInfoSpan

instance (Portioned a) => Portioned (Syntax.Module a) where
        getPortion = getPortion . Syntax.ann

instance Portioned Comments.Comment where
        getPortion (Comments.Comment _ commentPortion _) = commentPortion

getStartLine :: (SrcLoc.SrcInfo a) => a -> Line
getStartLine = Line . SrcLoc.startLine

getStartColumn :: (SrcLoc.SrcInfo a) => a -> Column
getStartColumn = Column . SrcLoc.startColumn

getEndLine :: SrcLoc.SrcSpan -> Line
getEndLine = Line . SrcLoc.srcSpanEndLine

getEndColumn :: SrcLoc.SrcSpan -> Column
getEndColumn = Column . SrcLoc.srcSpanEndColumn

createPosition :: FilePath -> Line -> Column -> SrcLoc.SrcLoc
createPosition file (Line line) (Column column)
  = SrcLoc.SrcLoc{SrcLoc.srcFilename = file, SrcLoc.srcLine = line,
                  SrcLoc.srcColumn = column}

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
        sumPredecessor difference = pred . add difference
        lineCount = length stringLines
        stringLines = Newlines.splitSeparatedLines string
        startLine = getStartLine startPosition
        endColumn = sumPredecessor lastLineLength lastLineStartColumn
        lastLineStartColumn = if hasSingleLine then startColumn else zero
        hasSingleLine = lineCount == 1
        startColumn = getStartColumn startPosition
        lastLineLength = length $ last stringLines

createComment ::
              Comment.Comment -> SrcLoc.SrcSpan -> Comments.Comment
createComment core portion
  = Comments.Comment isMultiLine portion content
  where isMultiLine
          = case Comment.kind core of
                Comment.Ordinary -> False
                Comment.Nested -> True
        content = Comment.content core

commentCore :: Comments.Comment -> Comment.Comment
commentCore core = Comment.create kind content
  where kind = commentKind core
        content = commentContent core

commentKind :: Comments.Comment -> Comment.Kind
commentKind (Comments.Comment False _ _) = Comment.Ordinary
commentKind (Comments.Comment True _ _) = Comment.Nested

commentContent :: Comments.Comment -> String
commentContent (Comments.Comment _ _ content) = content
