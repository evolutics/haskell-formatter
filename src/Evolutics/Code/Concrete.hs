module Evolutics.Code.Concrete
       (Commented, commentedRoot, comments, Commentless, commentlessRoot,
        createCommented, createCommentless, dropComments, createComment,
        isCommentMultiLine, commentPortion, commentContent)
       where
import qualified Language.Haskell.Exts.Annotated as Exts
import qualified Evolutics.Tools.Newlines as Newlines
import qualified Evolutics.Tools.SourceLocations as SourceLocations

data Commented = Commented{commentedRoot ::
                           Exts.Module Exts.SrcSpanInfo,
                           comments :: [Exts.Comment]}

data Commentless = Commentless{commentlessRoot ::
                               Exts.Module Exts.SrcSpanInfo}

instance Show Commented where
        show Commented{commentedRoot = root, comments = comments}
          = Exts.exactPrint root comments

createCommented ::
                Exts.Module Exts.SrcSpanInfo -> [Exts.Comment] -> Commented
createCommented root comments
  = Commented{commentedRoot = root, comments = comments}

createCommentless :: Exts.Module Exts.SrcSpanInfo -> Commentless
createCommentless root = Commentless{commentlessRoot = root}

dropComments :: Commented -> Commentless
dropComments Commented{commentedRoot = root}
  = createCommentless root

createComment :: Bool -> String -> Exts.SrcLoc -> Exts.Comment
createComment isMultiLine content startLocation
  = Exts.Comment isMultiLine portion content
  where portion = Exts.mkSrcSpan startLocation endLocation
        endLocation
          = Exts.SrcLoc{Exts.srcFilename = file, Exts.srcLine = endLine,
                        Exts.srcColumn = endColumn}
        file = Exts.fileName startLocation
        endLine = startLine + lineCount - 1
        startLine = Exts.startLine startLocation
        lineCount = length contentLines
        contentLines = Newlines.splitSeparatedLines content
        endColumn = contentEndColumn + if isMultiLine then 2 else 0
        contentEndColumn
          = lastContentLineStartColumn + lastContentLineLength - 1
        lastContentLineStartColumn
          = if lineCount == 1 then startColumn + 2 else 1
        startColumn = Exts.startColumn startLocation
        lastContentLineLength = length $ last contentLines

isCommentMultiLine :: Exts.Comment -> Bool
isCommentMultiLine (Exts.Comment isMultiLine _ _) = isMultiLine

commentPortion :: Exts.Comment -> Exts.SrcSpan
commentPortion = SourceLocations.portion

commentContent :: Exts.Comment -> String
commentContent (Exts.Comment _ _ content) = content
