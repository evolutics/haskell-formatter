{-|
Description : Comments without location
-}
module Language.Haskell.Formatter.CommentCore
       (CommentCore, kind, content, Kind(..), DocumentationDisplacement(..),
        create, wrappedLineCount, documentationDisplacement)
       where
import qualified Data.Char as Char
import qualified Data.Monoid as Monoid
import qualified Language.Haskell.Formatter.Internal.Newline as Newline
import qualified Language.Haskell.Formatter.Toolkit.ListTool as ListTool

data CommentCore = CommentCore{kind :: Kind, content :: String}
                     deriving (Eq, Ord)

data Kind = Ordinary
          | Nested
              deriving (Eq, Ord, Show)

data DocumentationDisplacement = BeforeActualCode
                               | AfterActualCode
                               | None
                                   deriving (Eq, Ord, Show)

instance Show CommentCore where
        show comment
          = case kind comment of
                Ordinary -> Monoid.mappend "--" rawContent
                Nested -> concat ["{-", rawContent, "-}"]
          where rawContent = content comment

create :: Kind -> String -> CommentCore
create rawKind rawContent = CommentCore{kind = rawKind, content = rawContent}

wrappedLineCount :: CommentCore -> Int
wrappedLineCount = length . Newline.splitSeparatedLines . show

documentationDisplacement :: CommentCore -> DocumentationDisplacement
documentationDisplacement comment
  = case unwrappedContent of
        ('|' : _) -> BeforeActualCode
        ('^' : _) -> AfterActualCode
        _ -> None
  where unwrappedContent
          = ListTool.dropWhileAtMost Char.isSpace spaceLimit $ content comment
        spaceLimit = 1
