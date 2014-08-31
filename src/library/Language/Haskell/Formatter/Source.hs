{-|
Description : Facade for HSE without location handling

See also "Language.Haskell.Formatter.Location".
-}
module Language.Haskell.Formatter.Source
       (Annotated.parseFileContents,
        Annotated.parseFileContentsWithComments, Comments.Comment,
        ExactPrint.exactPrint, Parser.defaultParseMode,
        Parser.parseFilename, Parser.ParseResult(..),
        module Language.Haskell.Exts.Pretty, (Syntax.=~=), Syntax.Module,
        createComment, commentCore)
       where
import Language.Haskell.Exts.Pretty
import qualified Language.Haskell.Exts.Annotated as Annotated
import qualified Language.Haskell.Exts.Annotated.ExactPrint
       as ExactPrint
import qualified Language.Haskell.Exts.Annotated.Syntax as Syntax
import qualified Language.Haskell.Exts.Comments as Comments
import qualified Language.Haskell.Exts.Parser as Parser
import qualified Language.Haskell.Formatter.CommentCore
       as CommentCore
import qualified Language.Haskell.Formatter.Location as Location

createComment ::
              CommentCore.CommentCore -> Location.SrcSpan -> Comments.Comment
createComment core portion
  = Comments.Comment isMultiLine portion content
  where isMultiLine
          = case CommentCore.kind core of
                CommentCore.Ordinary -> False
                CommentCore.Nested -> True
        content = CommentCore.content core

commentCore :: Comments.Comment -> CommentCore.CommentCore
commentCore comment = CommentCore.create kind content
  where kind = commentKind comment
        content = commentContent comment

commentKind :: Comments.Comment -> CommentCore.Kind
commentKind (Comments.Comment False _ _) = CommentCore.Ordinary
commentKind (Comments.Comment True _ _) = CommentCore.Nested

commentContent :: Comments.Comment -> String
commentContent (Comments.Comment _ _ content) = content
