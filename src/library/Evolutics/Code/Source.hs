module Evolutics.Code.Source
       (Annotated.parseFileContents,
        Annotated.parseFileContentsWithComments, Comments.Comment,
        ExactPrint.exactPrint, Parser.defaultParseMode,
        Parser.fromParseResult, Parser.parseFilename,
        Parser.ParseResult(..), Pretty.prettyPrint, (Syntax.=~=),
        Syntax.amap, Syntax.Module, createComment, commentCore)
       where
import qualified Language.Haskell.Exts.Annotated as Annotated
import qualified Language.Haskell.Exts.Annotated.ExactPrint
       as ExactPrint
import qualified Language.Haskell.Exts.Annotated.Syntax as Syntax
import qualified Language.Haskell.Exts.Comments as Comments
import qualified Language.Haskell.Exts.Parser as Parser
import qualified Language.Haskell.Exts.Pretty as Pretty
import qualified Evolutics.Code.Comment as Comment
import qualified Evolutics.Code.Location as Location

createComment ::
              Comment.Comment -> Location.SrcSpan -> Comments.Comment
createComment core portion
  = Comments.Comment isMultiLine portion content
  where isMultiLine
          = case Comment.kind core of
                Comment.Ordinary -> False
                Comment.Nested -> True
        content = Comment.content core

commentCore :: Comments.Comment -> Comment.Comment
commentCore core = Comment.create kind content
  where kind = commentKind core
        content = commentContent core

commentKind :: Comments.Comment -> Comment.Kind
commentKind (Comments.Comment False _ _) = Comment.Ordinary
commentKind (Comments.Comment True _ _) = Comment.Nested

commentContent :: Comments.Comment -> String
commentContent (Comments.Comment _ _ content) = content
