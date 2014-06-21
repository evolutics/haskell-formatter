module Evolutics.Code.Comment
       (Comment, kind, content, Kind(..), create, lineCount) where
import qualified Evolutics.Tools.Newlines as Newlines

data Comment = Comment{kind :: Kind, content :: String}

data Kind = Ordinary
          | Nested

create :: Kind -> String -> Comment
create commentKind commentContent
  = Comment{kind = commentKind, content = commentContent}

lineCount :: Comment -> Int
lineCount = length . Newlines.splitSeparatedLines . content
