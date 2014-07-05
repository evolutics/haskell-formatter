module Evolutics.Code.Concrete
       (Commented, commentedRoot, comments, Commentless, commentlessRoot,
        createCommented, createCommentless, makeCommentless, createComment,
        commentCore)
       where
import qualified Evolutics.Code.Comment as Comment
import qualified Evolutics.Code.Locations as Locations
import qualified Evolutics.Code.Source as Source

data Commented = Commented{commentedRoot ::
                           Source.Module Source.SrcSpanInfo,
                           comments :: [Source.Comment]}

data Commentless = Commentless{commentlessRoot ::
                               Source.Module Source.SrcSpanInfo}

instance Show Commented where
        show commented
          = Source.exactPrint (commentedRoot commented) $ comments commented

instance Locations.Portioned Commentless where
        portion = Locations.portion . commentlessRoot

createCommented ::
                Source.Module Source.SrcSpanInfo -> [Source.Comment] -> Commented
createCommented root commentList
  = Commented{commentedRoot = root, comments = commentList}

createCommentless ::
                  Source.Module Source.SrcSpanInfo -> Commentless
createCommentless root = Commentless{commentlessRoot = root}

makeCommentless :: Commented -> Commentless
makeCommentless = createCommentless . commentedRoot

createComment :: Comment.Comment -> Source.SrcLoc -> Source.Comment
createComment core startPosition
  = Source.Comment isMultiLine portion content
  where isMultiLine
          = case Comment.kind core of
                Comment.Ordinary -> False
                Comment.Nested -> True
        portion = Locations.stringPortion startPosition wrappedComment
        wrappedComment = show core
        content = Comment.content core

commentCore :: Source.Comment -> Comment.Comment
commentCore core = Comment.create kind content
  where kind = commentKind core
        content = commentContent core

commentKind :: Source.Comment -> Comment.Kind
commentKind (Source.Comment False _ _) = Comment.Ordinary
commentKind (Source.Comment True _ _) = Comment.Nested

commentContent :: Source.Comment -> String
commentContent (Source.Comment _ _ content) = content
