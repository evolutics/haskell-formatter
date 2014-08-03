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
import qualified Language.Haskell.Formatter.Result as Result

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
           Formatter -> Concrete.Commented -> Result.Result Concrete.Commented
formatCode formatter commented
  = do abstract <- checkedAssignComments formatter commented
       commentless <- checkedArrangeElements formatter $
                        Concrete.makeCommentless commented
       merged <- mergeCode abstract commentless
       merged' <- checkedFormatComments formatter merged
       commented' <- checkedIntegrateComments formatter merged'
       return commented'

checkedAssignComments ::
                      Formatter -> Concrete.Commented -> Result.Result Abstract.Code
checkedAssignComments formatter
  = transformAnnotations (assignComments formatter)
      Error.CommentAssignmentAssertion

transformAnnotations ::
                       (Coded a, Coded b) =>
                       (a -> b) -> Error.Error -> a -> Result.Result b
transformAnnotations transform unequalCase code
  = Result.check unequalCase code' isSameCode
  where code' = transform code
        isSameCode = getCode code' == getCode code

checkedArrangeElements ::
                       Formatter ->
                         Concrete.Commentless -> Result.Result Concrete.Commentless
checkedArrangeElements formatter
  = transformAnnotations (arrangeElements formatter)
      Error.ElementArrangementAssertion

mergeCode ::
          Abstract.Code -> Concrete.Commentless -> Result.Result Merged.Code
mergeCode abstract commentless
  = Result.checkMaybe Error.MergingAssertion $
      Merged.mergeCode abstract commentless

checkedFormatComments ::
                      Formatter -> Merged.Code -> Result.Result Merged.Code
checkedFormatComments formatter code
  = Result.check Error.CommentFormattingAssertion code' $
      Function.on (==) dropAnnotations code' code
  where dropAnnotations
          = fmap Merged.partNestedPortion . Merged.codeRoot
        code' = formatComments formatter code

checkedIntegrateComments ::
                         Formatter -> Merged.Code -> Result.Result Concrete.Commented
checkedIntegrateComments formatter
  = transformAnnotations (integrateComments formatter)
      Error.CommentIntegrationAssertion
