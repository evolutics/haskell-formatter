module Evolutics.Code.Concrete
       (Commented, commentedRoot, comments, Commentless, commentlessRoot,
        createCommented, createCommentless, dropComments, createComment,
        isCommentMultiLine, commentContent)
       where
import qualified Evolutics.Code.Core as Core
import qualified Evolutics.Tools.Newlines as Newlines
import qualified Evolutics.Tools.SourceLocations as SourceLocations

data Commented = Commented{commentedRoot ::
                           Core.Module Core.SrcSpanInfo,
                           comments :: [Core.Comment]}

data Commentless = Commentless{commentlessRoot ::
                               Core.Module Core.SrcSpanInfo}

instance Show Commented where
        show Commented{commentedRoot = root, comments = comments}
          = Core.exactPrint root comments

instance SourceLocations.Portioned Commentless where
        portion Commentless{commentlessRoot = root}
          = SourceLocations.portion $ Core.ann root

instance SourceLocations.Portioned Core.Comment where
        portion (Core.Comment _ portion _) = portion

createCommented ::
                Core.Module Core.SrcSpanInfo -> [Core.Comment] -> Commented
createCommented root comments
  = Commented{commentedRoot = root, comments = comments}

createCommentless :: Core.Module Core.SrcSpanInfo -> Commentless
createCommentless root = Commentless{commentlessRoot = root}

dropComments :: Commented -> Commentless
dropComments Commented{commentedRoot = root}
  = createCommentless root

createComment :: Bool -> String -> Core.SrcLoc -> Core.Comment
createComment isMultiLine content startPosition
  = Core.Comment isMultiLine portion content
  where portion = Core.mkSrcSpan startPosition endPosition
        endPosition
          = Core.SrcLoc{Core.srcFilename = file, Core.srcLine = endLine,
                        Core.srcColumn = endColumn}
        file = Core.fileName startPosition
        endLine = startLine + lineCount - 1
        startLine = Core.startLine startPosition
        lineCount = length contentLines
        contentLines = Newlines.splitSeparatedLines content
        endColumn = contentEndColumn + if isMultiLine then 2 else 0
        contentEndColumn
          = lastContentLineStartColumn + lastContentLineLength - 1
        lastContentLineStartColumn
          = if lineCount == 1 then startColumn + 2 else 1
        startColumn = Core.startColumn startPosition
        lastContentLineLength = length $ last contentLines

isCommentMultiLine :: Core.Comment -> Bool
isCommentMultiLine (Core.Comment isMultiLine _ _) = isMultiLine

commentContent :: Core.Comment -> String
commentContent (Core.Comment _ _ content) = content
