{-|
Description : Invocation of the process parts with assertion checking
-}
module Language.Haskell.Formatter.Process.Formatter (Formatter(..), format)
       where
import qualified Control.Monad as Monad
import qualified Data.Function as Function
import qualified Language.Haskell.Formatter.ExactCode as ExactCode
import qualified Language.Haskell.Formatter.Process.Code as Code
import qualified Language.Haskell.Formatter.Result as Result
import qualified Language.Haskell.Formatter.Source as Source

class Coded a where

        getCode :: a -> Source.Module ()

data Formatter = Formatter{attachComments ::
                           ExactCode.ExactCode ->
                             Result.Result Code.CommentableCode,
                           formatActualCode ::
                           Code.LocatableCommentableCode ->
                             Result.Result Code.LocatableCommentableCode,
                           formatComments ::
                           Code.LocatableCommentableCode ->
                             Result.Result Code.LocatableCommentableCode,
                           detachComments ::
                           Code.LocatableCommentableCode ->
                             Result.Result ExactCode.ExactCode}

instance Coded (Source.Module a) where
        getCode = Monad.void

instance Coded ExactCode.ExactCode where
        getCode = getCode . ExactCode.actualCode

format :: Formatter -> ExactCode.ExactCode -> Result.Result ExactCode.ExactCode
format formatter exact
  = do commentable <- checkedAttachComments formatter exact
       locatableCommentable <- Code.tryZipLocationsComments locatable
                                 commentable
       {- Formatting the actual code is allowed to change the code itself. -}
       locatableCommentable' <- formatActualCode formatter locatableCommentable
       locatableCommentable'' <- checkedFormatComments formatter
                                   locatableCommentable'
       exact' <- checkedDetachComments formatter locatableCommentable''
       return exact'
  where locatable = ExactCode.actualCode exact

checkedAttachComments ::
                      Formatter ->
                        ExactCode.ExactCode ->
                          Result.Result Code.CommentableCode
checkedAttachComments formatter
  = transformNotes (attachComments formatter) message
  where message = "Attaching the comments changed the code itself."

transformNotes ::
                 (Coded a, Coded b) =>
                 (a -> Result.Result b) -> String -> a -> Result.Result b
transformNotes transform = transformWithCheck transform assert
  where assert code code' = getCode code == getCode code'

transformWithCheck ::
                   (a -> Result.Result b) ->
                     (a -> b -> Bool) -> String -> a -> Result.Result b
transformWithCheck transform assert errorMessage input
  = do output <- transform input
       if assert input output then return output else
         Result.fatalAssertionError errorMessage

checkedFormatComments ::
                      Formatter ->
                        Code.LocatableCommentableCode ->
                          Result.Result Code.LocatableCommentableCode
checkedFormatComments formatter = transformWithCheck transform assert message
  where transform = formatComments formatter
        assert = Function.on (==) Code.dropComments
        message = "Formatting the comments changed more than just the comments."

checkedDetachComments ::
                      Formatter ->
                        Code.LocatableCommentableCode ->
                          Result.Result ExactCode.ExactCode
checkedDetachComments formatter
  = transformNotes (detachComments formatter) message
  where message = "Detaching the comments changed the code itself."
