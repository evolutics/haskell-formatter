module Language.Haskell.Formatter.Process.FormatComments
       (formatComments) where
import qualified Data.Function as Function
import qualified Language.Haskell.Formatter.Location as Location
import qualified Language.Haskell.Formatter.Process.Code as Code
import qualified Language.Haskell.Formatter.Process.Note as Note
import qualified Language.Haskell.Formatter.Result as Result
import qualified Language.Haskell.Formatter.Toolkit.Visit as Visit

formatComments ::
               Code.LocatableCommentableCode ->
                 Result.Result Code.LocatableCommentableCode
formatComments = return . indentToLineStart

indentToLineStart ::
                  Code.LocatableCommentableCode -> Code.LocatableCommentableCode
indentToLineStart locatableCommentable = locatableCommentable'
  where (_, locatableCommentable')
          = Visit.mapAccumulateLeftWithCreation indent startPosition
              locatableCommentable
        indent lineStart note
          = (lineStart', note{Note.commentNote = commentNote'})
          where lineStart'
                  = if Function.on (==) Location.getStartLine noteStart lineStart
                      then lineStart else noteStart
                noteStart = startPosition note
                commentNote' = Note.replaceCommentStartColumns replace commentNote
                replace = const $ Location.getStartColumn lineStart'
                commentNote = Note.commentNote note
        startPosition = Location.getPointLoc . Location.getPortion
