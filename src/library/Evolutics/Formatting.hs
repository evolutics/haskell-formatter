module Evolutics.Formatting (formatSource) where
import qualified Data.Maybe as Maybe
import qualified Evolutics.Code.Concrete as Concrete
import qualified Evolutics.Code.Helper as Helper
import qualified Evolutics.Code.Merged as Merged
import qualified Evolutics.Code.Source as Source
import qualified Evolutics.Transformations.CommentAssignment
       as CommentAssignment
import qualified Evolutics.Transformations.CommentFormatting
       as CommentFormatting
import qualified Evolutics.Transformations.CommentIntegration
       as CommentIntegration
import qualified Evolutics.Transformations.ElementArrangement
       as ElementArrangement

formatSource :: Maybe FilePath -> String -> Either String String
formatSource maybeFile
  = format . Source.parseFileContentsWithComments parseMode
  where format (Source.ParseFailed position message)
          = Left $ Helper.formatMessage position message
        format (Source.ParseOk (root, comments))
          = Right . show . formatCode $
              Concrete.createCommented root comments
        parseMode
          = case maybeFile of
                Nothing -> Source.defaultParseMode
                Just file -> Source.defaultParseMode{Source.parseFilename = file}

formatCode :: Concrete.Commented -> Concrete.Commented
formatCode commented = CommentIntegration.integrateComments merged'
  where merged' = CommentFormatting.formatComments merged
        merged
          = Maybe.fromMaybe (error unequalStructuresMessage) maybeMerged
        maybeMerged = Merged.mergeCode abstract commentless
        abstract = CommentAssignment.assignComments commented
        commentless
          = ElementArrangement.arrangeElements $
              Concrete.makeCommentless commented

unequalStructuresMessage :: String
unequalStructuresMessage
  = "The structures of the abstract and concrete code are unequal."