module Language.Haskell.Formatter.Code.Abstract
       (Code, codeRoot, Annotation, boxesBefore, boxesAfter, Box(..),
        Comment, commentCore, commentStartColumn, createCode,
        createAnnotation, createComment, mapCommentStartColumns)
       where
import qualified Data.Function as Function
import qualified Data.Monoid as Monoid
import qualified Language.Haskell.Formatter.Code.Comment as Comment
import qualified Language.Haskell.Formatter.Code.Location
       as Location
import qualified Language.Haskell.Formatter.Code.Source as Source

data Code = Code{codeRoot :: Source.Module Annotation}
          deriving (Eq, Ord, Show)

data Annotation = Annotation{boxesBefore :: [Box],
                             boxesAfter :: [Box]}
                deriving (Eq, Ord, Show)

data Box = CommentBox Comment
         | EmptyLine
         deriving (Eq, Ord, Show)

data Comment = Comment{commentCore :: Comment.Comment,
                       commentStartColumn :: Location.Column}
             deriving (Eq, Ord, Show)

instance Monoid.Monoid Annotation where
        mempty = createAnnotation [] []
        mappend left right = createAnnotation before after
          where before = merge boxesBefore
                merge getBoxes = Function.on (++) getBoxes left right
                after = merge boxesAfter

createCode :: Source.Module Annotation -> Code
createCode root = Code{codeRoot = root}

createAnnotation :: [Box] -> [Box] -> Annotation
createAnnotation before after
  = Annotation{boxesBefore = before, boxesAfter = after}

createComment :: Location.Column -> Comment.Comment -> Comment
createComment startColumn core
  = Comment{commentCore = core, commentStartColumn = startColumn}

mapBoxes :: (Box -> Box) -> Annotation -> Annotation
mapBoxes function annotation
  = annotation{boxesBefore = before', boxesAfter = after'}
  where before' = apply before
        apply = fmap function
        before = boxesBefore annotation
        after' = apply after
        after = boxesAfter annotation

mapComments :: (Comment -> Comment) -> Annotation -> Annotation
mapComments function = mapBoxes boxFunction
  where boxFunction (CommentBox comment)
          = CommentBox $ function comment
        boxFunction EmptyLine = EmptyLine

mapCommentStartColumns ::
                       (Location.Column -> Location.Column) -> Annotation -> Annotation
mapCommentStartColumns function = mapComments commentFunction
  where commentFunction comment
          = comment{commentStartColumn = startColumn'}
          where startColumn' = function startColumn
                startColumn = commentStartColumn comment
