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
import qualified Language.Haskell.Formatter.Toolkit.Visit as Visit

class Coded a where

        getCode :: a -> Source.Module ()

data Formatter = Formatter{assignComments ::
                           Concrete.Commented -> Result.Result Abstract.Code,
                           arrangeElements ::
                           Merged.Code -> Result.Result Concrete.Commentless,
                           formatComments :: Merged.Code -> Result.Result Merged.Code,
                           integrateComments ::
                           Merged.Code -> Result.Result Concrete.Commented}

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
       merged <- mergeCode abstract commentless
       commentless' <- checkedArrangeElements formatter merged
       merged' <- mergeCode abstract commentless'
       merged'' <- checkedFormatComments formatter merged'
       commented' <- checkedIntegrateComments formatter merged''
       return commented'
  where commentless = Concrete.makeCommentless commented

checkedAssignComments ::
                      Formatter -> Concrete.Commented -> Result.Result Abstract.Code
checkedAssignComments formatter
  = transformAnnotations (assignComments formatter)
      "The comment assignment changed the actual code."

transformAnnotations ::
                       (Coded a, Coded b) =>
                       (a -> Result.Result b) -> String -> a -> Result.Result b
transformAnnotations transform
  = transformWithCheck transform assert
  where assert code code' = getCode code == getCode code'

transformWithCheck ::
                   (a -> Result.Result b) ->
                     (a -> b -> Bool) -> String -> a -> Result.Result b
transformWithCheck transform assert errorMessage input
  = transform input >>= check
  where check output
          = Result.check assertionError output $ assert input output
        assertionError = Error.AssertionError errorMessage

mergeCode ::
          Abstract.Code -> Concrete.Commentless -> Result.Result Merged.Code
mergeCode abstract commentless
  = Result.checkMaybe assertionError maybeMerged
  where assertionError
          = Error.AssertionError "The code annotations could not be merged."
        maybeMerged = fmap Merged.createCode maybeMergedRoot
        maybeMergedRoot
          = Visit.halfZipWith Merged.createPart abstractRoot commentlessRoot
        abstractRoot = Abstract.codeRoot abstract
        commentlessRoot = Concrete.commentlessRoot commentless

checkedArrangeElements ::
                       Formatter -> Merged.Code -> Result.Result Concrete.Commentless
checkedArrangeElements formatter
  = transformAnnotations (arrangeElements formatter)
      "The element arrangement changed the actual code."

checkedFormatComments ::
                      Formatter -> Merged.Code -> Result.Result Merged.Code
checkedFormatComments formatter
  = transformWithCheck transform assert errorMessage
  where transform = formatComments formatter
        assert code code' = Function.on (==) dropAnnotations code code'
        dropAnnotations = fmap Merged.partNestedPortion . Merged.codeRoot
        errorMessage
          = "The comment formatting changed more than just the comments."

checkedIntegrateComments ::
                         Formatter -> Merged.Code -> Result.Result Concrete.Commented
checkedIntegrateComments formatter
  = transformAnnotations (integrateComments formatter)
      "The comment integration changed the actual code."
