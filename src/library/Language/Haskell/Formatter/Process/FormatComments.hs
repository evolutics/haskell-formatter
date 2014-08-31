module Language.Haskell.Formatter.Process.FormatComments (formatComments) where
import qualified Data.Function as Function
import qualified Language.Haskell.Formatter.Location as Location
import qualified Language.Haskell.Formatter.Process.Code as Code
import qualified Language.Haskell.Formatter.Process.Note as Note
import qualified Language.Haskell.Formatter.Result as Result
import qualified Language.Haskell.Formatter.Toolkit.ListTool as ListTool
import qualified Language.Haskell.Formatter.Toolkit.Visit as Visit

formatComments ::
               Code.LocatableCommentableCode ->
                 Result.Result Code.LocatableCommentableCode
formatComments = return . indentToLineStart . mergeConsecutiveEmptyLines

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

mergeConsecutiveEmptyLines ::
                           Code.LocatableCommentableCode ->
                             Code.LocatableCommentableCode
mergeConsecutiveEmptyLines
  = fmap . Note.replaceCommentNote $ Note.replaceCommentBoxes merge
  where merge = ListTool.mergeConsecutiveElements isMerged
        isMerged (Note.ActualComment _) = False
        isMerged Note.EmptyLine = True
