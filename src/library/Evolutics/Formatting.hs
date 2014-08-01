module Evolutics.Formatting (formatSource) where
import qualified Control.Monad as Monad
import qualified Data.Function as Function
import qualified Evolutics.Code.Abstract as Abstract
import qualified Evolutics.Code.Concrete as Concrete
import qualified Evolutics.Code.Helper as Helper
import qualified Evolutics.Code.Location as Location
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

class Coded a where

        getCode :: a -> Source.Module ()

instance Coded Abstract.Code where
        getCode = Monad.void . Abstract.codeRoot

instance Coded Concrete.Commented where
        getCode = Monad.void . Concrete.commentedRoot

instance Coded Concrete.Commentless where
        getCode = Monad.void . Concrete.commentlessRoot

instance Coded Merged.Code where
        getCode = Monad.void . Merged.codeRoot

data Error = ParseError Location.SrcLoc String
           | CommentAssignmentAssertion
           | ElementArrangementAssertion
           | MergingAssertion
           | CommentFormattingAssertion
           | CommentIntegrationAssertion
           deriving (Eq, Ord)

instance Show Error where
        show (ParseError position message)
          = Helper.formatMessage position message
        show CommentAssignmentAssertion
          = "Assertion error of comment assignment."
        show ElementArrangementAssertion
          = "Assertion error of element arrangement."
        show MergingAssertion = "Assertion error of merging."
        show CommentFormattingAssertion
          = "Assertion error of comment formatting."
        show CommentIntegrationAssertion
          = "Assertion error of comment integration."

formatSource :: Maybe FilePath -> String -> Either Error String
formatSource maybeFile
  = format . Source.parseFileContentsWithComments parseMode
  where format (Source.ParseFailed position message)
          = Left $ ParseError position message
        format (Source.ParseOk (root, comments))
          = formatCode code >>= Right . show
          where code = Concrete.createCommented root comments
        parseMode
          = case maybeFile of
                Nothing -> Source.defaultParseMode
                Just file -> Source.defaultParseMode{Source.parseFilename = file}

formatCode :: Concrete.Commented -> Either Error Concrete.Commented
formatCode commented
  = do abstract <- assignComments commented
       commentless <- arrangeElements $ Concrete.makeCommentless commented
       merged <- mergeCode abstract commentless
       merged' <- formatComments merged
       commented' <- integrateComments merged'
       return commented'

assignComments :: Concrete.Commented -> Either Error Abstract.Code
assignComments
  = transformAnnotations CommentAssignment.assignComments
      CommentAssignmentAssertion

transformAnnotations ::
                       (Coded a, Coded b) => (a -> b) -> c -> a -> Either c b
transformAnnotations transform unequalCase code
  = chooseEither unequalCase code' isSameCode
  where code' = transform code
        isSameCode = getCode code' == getCode code

chooseEither :: a -> b -> Bool -> Either a b
chooseEither left right isRight
  = if isRight then Right right else Left left

arrangeElements ::
                Concrete.Commentless -> Either Error Concrete.Commentless
arrangeElements
  = transformAnnotations ElementArrangement.arrangeElements
      ElementArrangementAssertion

mergeCode ::
          Abstract.Code -> Concrete.Commentless -> Either Error Merged.Code
mergeCode abstract commentless
  = case Merged.mergeCode abstract commentless of
        Nothing -> Left MergingAssertion
        Just merged -> Right merged

formatComments :: Merged.Code -> Either Error Merged.Code
formatComments code
  = chooseEither CommentFormattingAssertion code' $
      Function.on (==) dropAnnotations code' code
  where dropAnnotations
          = fmap Merged.partNestedPortion . Merged.codeRoot
        code' = CommentFormatting.formatComments code

integrateComments :: Merged.Code -> Either Error Concrete.Commented
integrateComments
  = transformAnnotations CommentIntegration.integrateComments
      CommentIntegrationAssertion
