module Evolutics.Formatting (formatSource) where
import qualified Language.Haskell.Exts.Annotated as Exts
import qualified Evolutics.Code.Concrete as Concrete
import qualified Evolutics.Tools.SourceLocations as SourceLocations
import qualified Evolutics.Transformations.CommentAssignment
       as CommentAssignment
import qualified Evolutics.Transformations.CommentIntegration
       as CommentIntegration
import qualified Evolutics.Transformations.ElementArrangement
       as ElementArrangement

formatSource :: Maybe FilePath -> String -> Either String String
formatSource maybeFile
  = format . Exts.parseFileContentsWithComments parseMode
  where format (Exts.ParseFailed position message)
          = Left $ SourceLocations.formatMessage position message
        format (Exts.ParseOk (root, comments))
          = Right . show . formatCode $
              Concrete.createCommented root comments
        parseMode
          = case maybeFile of
                Nothing -> Exts.defaultParseMode
                Just file -> Exts.defaultParseMode{Exts.parseFilename = file}

formatCode :: Concrete.Commented -> Concrete.Commented
formatCode concreteCommented
  = CommentIntegration.integrateComments abstract concreteCommentless
  where abstract = CommentAssignment.assignComments concreteCommented
        concreteCommentless
          = ElementArrangement.arrangeElements $
              Concrete.dropComments concreteCommented
