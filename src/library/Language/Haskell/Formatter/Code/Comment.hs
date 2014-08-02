module Language.Haskell.Formatter.Code.Comment
       (Comment, kind, content, Kind(..), AnnotationDisplacement(..),
        create, wrappedLines, annotationDisplacement)
       where
import qualified Data.Char as Char
import qualified Data.Monoid as Monoid
import qualified Language.Haskell.Formatter.Toolkit.Newline
       as Newline

data Comment = Comment{kind :: Kind, content :: String}
             deriving (Eq, Ord)

data Kind = Ordinary
          | Nested
          deriving (Eq, Ord, Show)

data AnnotationDisplacement = BeforeElement
                            | AfterElement
                            | None
                            deriving (Eq, Ord, Show)

instance Show Comment where
        show comment
          = case kind comment of
                Ordinary -> Monoid.mappend "--" commentContent
                Nested -> Monoid.mconcat ["{-", commentContent, "-}"]
          where commentContent = content comment

create :: Kind -> String -> Comment
create commentKind commentContent
  = Comment{kind = commentKind, content = commentContent}

wrappedLines :: Comment -> [String]
wrappedLines = Newline.splitSeparatedLines . show

annotationDisplacement :: Comment -> AnnotationDisplacement
annotationDisplacement comment
  = case unwrappedContent of
        ('|' : _) -> BeforeElement
        ('^' : _) -> AfterElement
        _ -> None
  where unwrappedContent
          = dropWhile Char.isSpace $ Monoid.mappend left right
        (left, right) = splitAt spaceLimit $ content comment
        spaceLimit = 1
