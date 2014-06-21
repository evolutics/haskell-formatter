module Evolutics.Code.Concrete
       (Commented, commentedRoot, comments, Commentless, commentlessRoot,
        createCommented, createCommentless, dropComments, createComment,
        commentKind, commentContent)
       where
import qualified Evolutics.Code.Comment as Comment
import qualified Evolutics.Code.Core as Core
import qualified Evolutics.Code.Locations as Locations
import qualified Evolutics.Tools.Newlines as Newlines

data Commented = Commented{commentedRoot ::
                           Core.Module Core.SrcSpanInfo,
                           comments :: [Core.Comment]}

data Commentless = Commentless{commentlessRoot ::
                               Core.Module Core.SrcSpanInfo}

instance Show Commented where
        show Commented{commentedRoot = root, comments = comments}
          = Core.exactPrint root comments

instance Locations.Portioned Commentless where
        portion Commentless{commentlessRoot = root}
          = Locations.portion $ Core.ann root

instance Locations.Portioned Core.Comment where
        portion (Core.Comment _ commentPortion _) = commentPortion

createCommented ::
                Core.Module Core.SrcSpanInfo -> [Core.Comment] -> Commented
createCommented root comments
  = Commented{commentedRoot = root, comments = comments}

createCommentless :: Core.Module Core.SrcSpanInfo -> Commentless
createCommentless root = Commentless{commentlessRoot = root}

dropComments :: Commented -> Commentless
dropComments Commented{commentedRoot = root}
  = createCommentless root

createComment ::
              Comment.Kind -> String -> Core.SrcLoc -> Core.Comment
createComment kind content startPosition
  = Core.Comment isMultiLine portion content
  where isMultiLine
          = case kind of
                Comment.Ordinary -> False
                Comment.Nested -> True
        portion = Locations.stringPortion startPosition wrappedComment
        wrappedComment = show $ Comment.create kind content

commentKind :: Core.Comment -> Comment.Kind
commentKind (Core.Comment False _ _) = Comment.Ordinary
commentKind (Core.Comment True _ _) = Comment.Nested

commentContent :: Core.Comment -> String
commentContent (Core.Comment _ _ content) = content
