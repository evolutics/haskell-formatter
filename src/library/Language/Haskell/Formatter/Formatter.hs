module Language.Haskell.Formatter.Formatter
       (Formatter(..), formatCode) where
import qualified Control.Monad as Monad
import qualified Data.Function as Function
import qualified Language.Haskell.Formatter.Code.Abstract
       as Abstract
import qualified Language.Haskell.Formatter.Code.Concrete
       as Concrete
import qualified Language.Haskell.Formatter.Code.Merged as Merged
import qualified Language.Haskell.Formatter.Code.Source as Source
import qualified Language.Haskell.Formatter.Error as Error

class Coded a where

        getCode :: a -> Source.Module ()

data Formatter = Formatter{assignComments ::
                           Concrete.Commented -> Abstract.Code,
                           arrangeElements :: Concrete.Commentless -> Concrete.Commentless,
                           formatComments :: Merged.Code -> Merged.Code,
                           integrateComments :: Merged.Code -> Concrete.Commented}

instance Coded Abstract.Code where
        getCode = Monad.void . Abstract.codeRoot

instance Coded Concrete.Commented where
        getCode = Monad.void . Concrete.commentedRoot

instance Coded Concrete.Commentless where
        getCode = Monad.void . Concrete.commentlessRoot

instance Coded Merged.Code where
        getCode = Monad.void . Merged.codeRoot

formatCode ::
           Formatter ->
             Concrete.Commented -> Either Error.Error Concrete.Commented
formatCode formatter commented
  = do abstract <- checkedAssignComments formatter commented
       commentless <- checkedArrangeElements formatter $
                        Concrete.makeCommentless commented
       merged <- mergeCode abstract commentless
       merged' <- checkedFormatComments formatter merged
       commented' <- checkedIntegrateComments formatter merged'
       return commented'

checkedAssignComments ::
                      Formatter -> Concrete.Commented -> Either Error.Error Abstract.Code
checkedAssignComments formatter
  = transformAnnotations (assignComments formatter)
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

checkedArrangeElements ::
                       Formatter ->
                         Concrete.Commentless -> Either Error.Error Concrete.Commentless
checkedArrangeElements formatter
  = transformAnnotations (arrangeElements formatter)
      Error.ElementArrangementAssertion

mergeCode ::
          Abstract.Code ->
            Concrete.Commentless -> Either Error.Error Merged.Code
mergeCode abstract commentless
  = case Merged.mergeCode abstract commentless of
        Nothing -> Left Error.MergingAssertion
        Just merged -> Right merged

checkedFormatComments ::
                      Formatter -> Merged.Code -> Either Error.Error Merged.Code
checkedFormatComments formatter code
  = chooseEither Error.CommentFormattingAssertion code' $
      Function.on (==) dropAnnotations code' code
  where dropAnnotations
          = fmap Merged.partNestedPortion . Merged.codeRoot
        code' = formatComments formatter code

checkedIntegrateComments ::
                         Formatter -> Merged.Code -> Either Error.Error Concrete.Commented
checkedIntegrateComments formatter
  = transformAnnotations (integrateComments formatter)
      Error.CommentIntegrationAssertion
