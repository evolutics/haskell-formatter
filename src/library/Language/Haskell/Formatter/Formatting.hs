module Language.Haskell.Formatter.Formatting (formatSource) where
import qualified Control.Monad as Monad
import qualified Data.Function as Function
import qualified Language.Haskell.Formatter.Code.Abstract
       as Abstract
import qualified Language.Haskell.Formatter.Code.Concrete
       as Concrete
import qualified Language.Haskell.Formatter.Code.Merged as Merged
import qualified Language.Haskell.Formatter.Code.Source as Source
import qualified
       Language.Haskell.Formatter.Default.CommentAssignment
       as CommentAssignment
import qualified
       Language.Haskell.Formatter.Default.CommentFormatting
       as CommentFormatting
import qualified
       Language.Haskell.Formatter.Default.CommentIntegration
       as CommentIntegration
import qualified
       Language.Haskell.Formatter.Default.ElementArrangement
       as ElementArrangement
import qualified Language.Haskell.Formatter.Error as Error

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

formatSource ::
             Maybe FilePath -> String -> Either Error.Error String
formatSource maybeFile
  = format . Source.parseFileContentsWithComments parseMode
  where format (Source.ParseFailed position message)
          = Left $ Error.ParseError position message
        format (Source.ParseOk (root, comments))
          = formatCode code >>= Right . show
          where code = Concrete.createCommented root comments
        parseMode
          = case maybeFile of
                Nothing -> Source.defaultParseMode
                Just file -> Source.defaultParseMode{Source.parseFilename = file}

formatCode ::
           Concrete.Commented -> Either Error.Error Concrete.Commented
formatCode commented
  = do abstract <- assignComments commented
       commentless <- arrangeElements $ Concrete.makeCommentless commented
       merged <- mergeCode abstract commentless
       merged' <- formatComments merged
       commented' <- integrateComments merged'
       return commented'

assignComments ::
               Concrete.Commented -> Either Error.Error Abstract.Code
assignComments
  = transformAnnotations CommentAssignment.assignComments
      Error.CommentAssignmentAssertion

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
                Concrete.Commentless -> Either Error.Error Concrete.Commentless
arrangeElements
  = transformAnnotations ElementArrangement.arrangeElements
      Error.ElementArrangementAssertion

mergeCode ::
          Abstract.Code ->
            Concrete.Commentless -> Either Error.Error Merged.Code
mergeCode abstract commentless
  = case Merged.mergeCode abstract commentless of
        Nothing -> Left Error.MergingAssertion
        Just merged -> Right merged

formatComments :: Merged.Code -> Either Error.Error Merged.Code
formatComments code
  = chooseEither Error.CommentFormattingAssertion code' $
      Function.on (==) dropAnnotations code' code
  where dropAnnotations
          = fmap Merged.partNestedPortion . Merged.codeRoot
        code' = CommentFormatting.formatComments code

integrateComments ::
                  Merged.Code -> Either Error.Error Concrete.Commented
integrateComments
  = transformAnnotations CommentIntegration.integrateComments
      Error.CommentIntegrationAssertion
