module Evolutics.Code.Abstract
       (Code, codeRoot, Annotation, commentsBefore, commentsAfter,
        Comment, commentCore, commentStartColumn, createCode,
        createAnnotation, createComment, mapCommentStartColumns)
       where
import qualified Evolutics.Code.Comment as Comment
import qualified Evolutics.Code.Location as Location
import qualified Evolutics.Code.Source as Source

data Code = Code{codeRoot :: Source.Module Annotation}
          deriving (Eq, Ord, Show)

data Annotation = Annotation{commentsBefore :: [Comment],
                             commentsAfter :: [Comment]}
                deriving (Eq, Ord, Show)

data Comment = Comment{commentCore :: Comment.Comment,
                       commentStartColumn :: Location.Column}
             deriving (Eq, Ord, Show)

createCode :: Source.Module Annotation -> Code
createCode root = Code{codeRoot = root}

createAnnotation :: [Comment] -> [Comment] -> Annotation
createAnnotation before after
  = Annotation{commentsBefore = before, commentsAfter = after}

createComment :: Location.Column -> Comment.Comment -> Comment
createComment startColumn core
  = Comment{commentCore = core, commentStartColumn = startColumn}

mapComments :: (Comment -> Comment) -> Annotation -> Annotation
mapComments function annotation
  = annotation{commentsBefore = before', commentsAfter = after'}
  where before' = apply before
        apply = map function
        before = commentsBefore annotation
        after' = apply after
        after = commentsAfter annotation

mapCommentStartColumns ::
                       (Location.Column -> Location.Column) -> Annotation -> Annotation
mapCommentStartColumns function = mapComments mapComment
  where mapComment comment
          = comment{commentStartColumn = startColumn'}
          where startColumn' = function startColumn
                startColumn = commentStartColumn comment
