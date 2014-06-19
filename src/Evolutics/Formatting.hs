module Evolutics.Formatting (formatSource) where
import qualified Evolutics.Code.Concrete as Concrete
import qualified Evolutics.Code.Core as Core
import qualified Evolutics.Tools.SourceLocations as SourceLocations
import qualified Evolutics.Transformations.CommentAssignment
       as CommentAssignment
import qualified Evolutics.Transformations.CommentIntegration
       as CommentIntegration
import qualified Evolutics.Transformations.ElementArrangement
       as ElementArrangement

formatSource :: Maybe FilePath -> String -> Either String String
formatSource maybeFile
  = format . Core.parseFileContentsWithComments parseMode
  where format (Core.ParseFailed position message)
          = Left $ SourceLocations.formatMessage position message
        format (Core.ParseOk (root, comments))
          = Right . show . formatCode $
              Concrete.createCommented root comments
        parseMode
          = case maybeFile of
                Nothing -> Core.defaultParseMode
                Just file -> Core.defaultParseMode{Core.parseFilename = file}

formatCode :: Concrete.Commented -> Concrete.Commented
formatCode concreteCommented
  = CommentIntegration.integrateComments abstract concreteCommentless
  where abstract = CommentAssignment.assignComments concreteCommented
        concreteCommentless
          = ElementArrangement.arrangeElements $
              Concrete.dropComments concreteCommented
