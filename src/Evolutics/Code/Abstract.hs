module Evolutics.Code.Abstract
       (Code, codeRoot, Annotation, commentsBefore, commentsAfter,
        Comment, commentCore, commentStartColumn, createCode,
        createAnnotation, createComment)
       where
import qualified Evolutics.Code.Comment as Comment
import qualified Evolutics.Code.Core as Core
import qualified Evolutics.Code.Locations as Locations

data Code = Code{codeRoot :: Core.Module Annotation}

data Annotation = Annotation{commentsBefore :: [Comment],
                             commentsAfter :: [Comment]}

data Comment = Comment{commentCore :: Comment.Comment,
                       commentStartColumn :: Locations.Column}

createCode :: Core.Module Annotation -> Code
createCode root = Code{codeRoot = root}

createAnnotation :: [Comment] -> [Comment] -> Annotation
createAnnotation before after
  = Annotation{commentsBefore = before, commentsAfter = after}

createComment :: Locations.Column -> Comment.Comment -> Comment
createComment startColumn core
  = Comment{commentCore = core, commentStartColumn = startColumn}
