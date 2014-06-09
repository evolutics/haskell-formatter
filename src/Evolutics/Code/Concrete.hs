module Evolutics.Code.Concrete
       (Commented, commentedRoot, comments, Commentless, commentlessRoot,
        createCommented, createCommentless, dropComments,
        isCommentMultiLine, commentPortion, commentContent)
       where
import qualified Language.Haskell.Exts.Annotated as Exts
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

isCommentMultiLine :: Exts.Comment -> Bool
isCommentMultiLine (Exts.Comment isMultiLine _ _) = isMultiLine

commentPortion :: Exts.Comment -> Exts.SrcSpan
commentPortion = SourceLocations.portion

commentContent :: Exts.Comment -> String
commentContent (Exts.Comment _ _ content) = content
