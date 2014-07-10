module Evolutics.Code.Comment
       (Comment, kind, content, Kind(..), AnnotationDisplacement(..),
        create, wrappedLines, annotationDisplacement)
       where
import qualified Data.Char as Char
import qualified Evolutics.Tools.Newlines as Newlines

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
                Ordinary -> "--" ++ commentContent
                Nested -> "{-" ++ commentContent ++ "-}"
          where commentContent = content comment

create :: Kind -> String -> Comment
create commentKind commentContent
  = Comment{kind = commentKind, content = commentContent}

wrappedLines :: Comment -> [String]
wrappedLines = Newlines.splitSeparatedLines . show

annotationDisplacement :: Comment -> AnnotationDisplacement
annotationDisplacement comment
  = case unwrappedContent of
        ('|' : _) -> BeforeElement
        ('^' : _) -> AfterElement
        _ -> None
  where unwrappedContent = dropWhile Char.isSpace left ++ right
        (left, right) = splitAt spaceLimit $ content comment
        spaceLimit = 1
