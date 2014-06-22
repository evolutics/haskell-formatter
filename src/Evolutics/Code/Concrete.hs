module Evolutics.Code.Concrete
       (Commented, commentedRoot, comments, Commentless, commentlessRoot,
        createCommented, createCommentless, makeCommentless, createComment,
        commentCore)
       where
import qualified Evolutics.Code.Comment as Comment
import qualified Evolutics.Code.Core as Core
import qualified Evolutics.Code.Locations as Locations

data Commented = Commented{commentedRoot ::
                           Core.Module Core.SrcSpanInfo,
                           comments :: [Core.Comment]}

data Commentless = Commentless{commentlessRoot ::
                               Core.Module Core.SrcSpanInfo}

instance Show Commented where
        show commented
          = Core.exactPrint (commentedRoot commented) $ comments commented

instance Locations.Portioned Commentless where
        portion = Locations.portion . commentlessRoot

instance Locations.Portioned Core.Comment where
        portion (Core.Comment _ commentPortion _) = commentPortion

createCommented ::
                Core.Module Core.SrcSpanInfo -> [Core.Comment] -> Commented
createCommented root comments
  = Commented{commentedRoot = root, comments = comments}

createCommentless :: Core.Module Core.SrcSpanInfo -> Commentless
createCommentless root = Commentless{commentlessRoot = root}

makeCommentless :: Commented -> Commentless
makeCommentless = createCommentless . commentedRoot

createComment :: Comment.Comment -> Core.SrcLoc -> Core.Comment
createComment core startPosition
  = Core.Comment isMultiLine portion content
  where isMultiLine
          = case Comment.kind core of
                Comment.Ordinary -> False
                Comment.Nested -> True
        portion = Locations.stringPortion startPosition wrappedComment
        wrappedComment = show core
        content = Comment.content core

commentCore :: Core.Comment -> Comment.Comment
commentCore core = Comment.create kind content
  where kind = commentKind core
        content = commentContent core

commentKind :: Core.Comment -> Comment.Kind
commentKind (Core.Comment False _ _) = Comment.Ordinary
commentKind (Core.Comment True _ _) = Comment.Nested

commentContent :: Core.Comment -> String
commentContent (Core.Comment _ _ content) = content
