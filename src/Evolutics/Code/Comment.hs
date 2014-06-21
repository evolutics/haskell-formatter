module Evolutics.Code.Comment
       (Comment, kind, content, Kind(..), create, contentLineCount) where
import qualified Evolutics.Tools.Newlines as Newlines

data Comment = Comment{kind :: Kind, content :: String}

data Kind = Ordinary
          | Nested

instance Show Comment where
        show Comment{kind = Ordinary, content = commentContent}
          = "--" ++ commentContent
        show Comment{kind = Nested, content = commentContent}
          = "{-" ++ commentContent ++ "-}"

create :: Kind -> String -> Comment
create commentKind commentContent
  = Comment{kind = commentKind, content = commentContent}

contentLineCount :: Comment -> Int
contentLineCount = length . Newlines.splitSeparatedLines . content
