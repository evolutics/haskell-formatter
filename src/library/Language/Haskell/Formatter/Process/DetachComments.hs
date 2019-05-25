{-|
Description : Detaching comments from the annotations of the syntax tree

This is the last part of the process.
-}
module Language.Haskell.Formatter.Process.DetachComments (detachComments) where
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Monoid as Monoid
import qualified Language.Haskell.Formatter.CommentCore as CommentCore
import qualified Language.Haskell.Formatter.ExactCode as ExactCode
import qualified Language.Haskell.Formatter.Location as Location
import qualified Language.Haskell.Formatter.Process.Code as Code
import qualified Language.Haskell.Formatter.Process.LineTool as LineTool
import qualified Language.Haskell.Formatter.Process.Note as Note
import qualified Language.Haskell.Formatter.Result as Result
import qualified Language.Haskell.Formatter.Source as Source
import qualified Language.Haskell.Formatter.Style as Style
import qualified Language.Haskell.Formatter.Toolkit.ListTool as ListTool
import qualified Language.Haskell.Formatter.Toolkit.StreamName as StreamName

newtype Reservation = Reservation (Map.Map Location.Line [Note.CommentBox])
                        deriving (Eq, Ord, Show)

instance Monoid.Monoid Reservation where
        mempty = Reservation Map.empty
        mappend (Reservation left) (Reservation right) = Reservation merged
          where merged = Map.unionWith merge left right
                merge before after = concat [before, between, after]
                  where between
                          = [Note.EmptyLine |
                             hasActualComment (ListTool.maybeLast before) &&
                               hasActualComment (Maybe.listToMaybe after)]
                hasActualComment maybeComment
                  = case maybeComment of
                        Nothing -> False
                        Just (Note.ActualComment _) -> True
                        Just Note.EmptyLine -> False

detachComments ::
               Style.Style ->
                 Code.LocatableCommentableCode ->
                   Result.Result ExactCode.ExactCode
detachComments _ locatableCommentable
  = return $ ExactCode.create locatable' comments
  where locatable' = LineTool.shiftCode shifter locatable
        shifter = reservationShifter reservation
        reservation = reserveForCode locatableCommentable
        locatable = Code.dropComments locatableCommentable
        comments = createComments stream reservation
        stream = Location.streamName $ Location.getPortion locatable'

reservationShifter :: Reservation -> LineTool.Shifter
reservationShifter (Reservation reservation)
  = LineTool.createShifter $ fmap commentsShift reservation

commentsShift :: [Note.CommentBox] -> LineTool.Shift
commentsShift = sum . fmap commentShift

commentShift :: Note.CommentBox -> LineTool.Shift
commentShift (Note.ActualComment comment)
  = CommentCore.wrappedLineCount $ Note.commentCore comment
commentShift Note.EmptyLine = 1

reserveForCode :: Code.LocatableCommentableCode -> Reservation
reserveForCode = Foldable.foldMap reserveForNote

reserveForNote :: Note.LocationCommentNote -> Reservation
reserveForNote note = Monoid.mappend before after
  where before = singleton lineBefore $ Note.commentsBefore commentNote
        singleton line = Reservation . Map.singleton line
        lineBefore = Location.getStartLine portion
        portion = Location.getPortion note
        commentNote = Note.commentNote note
        after = singleton lineAfter $ Note.commentsAfter commentNote
        lineAfter = succ $ Location.getEndLine portion

createComments :: StreamName.StreamName -> Reservation -> [Source.Comment]
createComments stream = accumulateReservation create
  where create baseLine = snd . List.foldl' merge (baseLine, [])
        merge (startLine, comments) box = (followingLine, comments')
          where followingLine = Location.plus shift startLine
                shift = commentShift box
                comments' = Monoid.mappend comments commentsNow
                commentsNow
                  = case box of
                        Note.ActualComment comment -> [createComment stream
                                                         startLine
                                                         comment]
                        Note.EmptyLine -> []

accumulateReservation ::
                        Monoid.Monoid m =>
                        (Location.Line -> [Note.CommentBox] -> m) ->
                          Reservation -> m
accumulateReservation create (Reservation reservation) = accumulation
  where (_, accumulation) = Map.foldlWithKey' accumulate base reservation
        accumulate (absoluteShift, structure) line comments
          = (absoluteShift', structure')
          where absoluteShift' = absoluteShift + relativeShift
                relativeShift = commentsShift comments
                structure' = Monoid.mappend structure part
                part = create shiftedLine comments
                shiftedLine = Location.plus absoluteShift line
        base = (noShift, Monoid.mempty)
        noShift = 0

createComment ::
              StreamName.StreamName ->
                Location.Line -> Note.IndentedComment -> Source.Comment
createComment stream startLine comment = Source.createComment core portion
  where core = Note.commentCore comment
        portion = Location.stringPortion startPosition wrappedComment
        startPosition = Location.createPosition stream startLine startColumn
        startColumn = Note.commentStartColumn comment
        wrappedComment = show core
