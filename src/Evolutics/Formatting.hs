module Evolutics.Formatting (formatSource) where
import qualified Language.Haskell.Exts.Annotated as Exts
import qualified Evolutics.Code.ConcreteCommented
       as ConcreteCommented
import qualified Evolutics.Tools as Tools
import qualified Evolutics.Transformations.CommentAssignment
       as CommentAssignment
import qualified Evolutics.Transformations.CommentIntegration
       as CommentIntegration
import qualified Evolutics.Transformations.ElementArrangement
       as ElementArrangement

formatSource :: Maybe FilePath -> String -> Either String String
formatSource maybeFile
  = format . Exts.parseFileContentsWithComments parseMode
  where format (Exts.ParseFailed location message)
          = Left $ Tools.formatSourceMessage location message
        format (Exts.ParseOk (root, comments))
          = Right . show . formatCode $
              ConcreteCommented.create root comments
        parseMode
          = case maybeFile of
                Nothing -> Exts.defaultParseMode
                Just file -> Exts.defaultParseMode{Exts.parseFilename = file}

formatCode ::
           ConcreteCommented.ConcreteCommented ->
             ConcreteCommented.ConcreteCommented
formatCode concreteCommented
  = CommentIntegration.integrateComments abstract concreteCommentless
  where abstract = CommentAssignment.assignComments concreteCommented
        concreteCommentless
          = ElementArrangement.arrangeElements $
              ConcreteCommented.dropComments concreteCommented
