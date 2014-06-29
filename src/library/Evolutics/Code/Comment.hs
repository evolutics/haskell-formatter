module Evolutics.Code.Comment
       (Comment, kind, content, Kind(..), create, wrappedLines) where
import qualified Evolutics.Tools.Newlines as Newlines

data Comment = Comment{kind :: Kind, content :: String}

data Kind = Ordinary
          | Nested

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
