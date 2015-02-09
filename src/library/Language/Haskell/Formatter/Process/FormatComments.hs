{-|
Description : Rearranging the comments
-}
module Language.Haskell.Formatter.Process.FormatComments (formatComments) where
import qualified Data.Function as Function
import qualified Data.Monoid as Monoid
import qualified Language.Haskell.Formatter.ExactCode as ExactCode
import qualified Language.Haskell.Formatter.Location as Location
import qualified Language.Haskell.Formatter.Process.AttachComments
       as AttachComments
import qualified Language.Haskell.Formatter.Process.Code as Code
import qualified Language.Haskell.Formatter.Process.Note as Note
import qualified Language.Haskell.Formatter.Result as Result
import qualified Language.Haskell.Formatter.Style as Style
import qualified Language.Haskell.Formatter.Toolkit.ListTool as ListTool
import qualified Language.Haskell.Formatter.Toolkit.Visit as Visit

formatComments ::
               Style.Style ->
                 Code.LocatableCommentableCode ->
                   Result.Result Code.LocatableCommentableCode
formatComments style locatableCommentable
  = do locatableCommentable' <- mergeImpliedComments style locatableCommentable
       return . indentToLineStart $
         mergeSuccessiveEmptyLines style locatableCommentable'

mergeImpliedComments ::
                     Style.Style ->
                       Code.LocatableCommentableCode ->
                         Result.Result Code.LocatableCommentableCode
mergeImpliedComments style locatableCommentable
  = do impliedCommentable <- commentsImpliedByLocations style locatable
       commentable' <- commentsDifference style commentable impliedCommentable
       Code.tryZipLocationsComments locatable commentable'
  where locatable = Code.dropComments locatableCommentable
        commentable = Code.dropLocations locatableCommentable

commentsImpliedByLocations ::
                           Style.Style ->
                             Code.LocatableCode ->
                               Result.Result Code.CommentableCode
commentsImpliedByLocations style locatable
  = AttachComments.attachComments style exact
  where exact = ExactCode.create locatable comments
        comments = []

commentsDifference ::
                   Style.Style ->
                     Code.CommentableCode ->
                       Code.CommentableCode ->
                         Result.Result Code.CommentableCode
commentsDifference style = Code.tryZipCode minus
  where minus mixed implied
          = Note.createCommentNote commentsBefore commentsAfter
          where commentsBefore = difference Note.commentsBefore
                difference getComments
                  = Function.on (boxesDifference style) getComments mixed
                      implied
                commentsAfter
                  = reverse . difference $ reverse . Note.commentsAfter

boxesDifference ::
                Style.Style ->
                  [Note.CommentBox] -> [Note.CommentBox] -> [Note.CommentBox]
boxesDifference style mixed implied = Monoid.mappend difference mixedRest
  where difference = replicate differenceCount Note.EmptyLine
        differenceCount
          = if mixedCount <= impliedCount then 0 else
              clip mixedCount - impliedCount
        mixedCount = length mixedEmptyLines
        (mixedEmptyLines, mixedRest) = span isEmptyLine mixed
        impliedCount = length implied
        clip = min successiveEmptyLinesLimit
        successiveEmptyLinesLimit = Style.successiveEmptyLinesLimit style

isEmptyLine :: Note.CommentBox -> Bool
isEmptyLine (Note.ActualComment _) = False
isEmptyLine Note.EmptyLine = True

indentToLineStart ::
                  Code.LocatableCommentableCode -> Code.LocatableCommentableCode
indentToLineStart locatableCommentable = locatableCommentable'
  where (_, locatableCommentable')
          = Visit.mapAccumulateLeftWithCreation move startPosition
              locatableCommentable
        move lineStart note = (lineStart', Note.replaceCommentNote replace note)
          where lineStart'
                  = if
                      Function.on (==) Location.getStartLine noteStart lineStart
                      then lineStart else noteStart
                noteStart = startPosition note
                replace = Note.replaceCommentStartColumn indent
                indent = const $ Location.getStartColumn lineStart'
        startPosition = Location.getPointLoc . Location.getPortion

mergeSuccessiveEmptyLines ::
                          Style.Style ->
                            Code.LocatableCommentableCode ->
                              Code.LocatableCommentableCode
mergeSuccessiveEmptyLines style
  = fmap . Note.replaceCommentNote $ Note.replaceCommentBoxes merge
  where merge
          = ListTool.mergeLongerSuccessions isEmptyLine
              successiveEmptyLinesLimit
        successiveEmptyLinesLimit = Style.successiveEmptyLinesLimit style
