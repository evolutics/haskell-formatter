module Language.Haskell.Formatter.Default.CommentIntegration
       (integrateComments) where
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Monoid as Monoid
import qualified Language.Haskell.Formatter.Code.Abstract
       as Abstract
import qualified Language.Haskell.Formatter.Code.Comment as Comment
import qualified Language.Haskell.Formatter.Code.Concrete
       as Concrete
import qualified Language.Haskell.Formatter.Code.Location
       as Location
import qualified Language.Haskell.Formatter.Code.Merged as Merged
import qualified Language.Haskell.Formatter.Code.Source as Source
import qualified Language.Haskell.Formatter.Default.Shifting
       as Shifting
import qualified Language.Haskell.Formatter.Toolkit.ListTool
       as ListTool

data Reservation = Reservation (Map.Map Location.Line
                                  [Abstract.Box])
                 deriving (Eq, Ord, Show)

instance Monoid.Monoid Reservation where
        mempty = Reservation Map.empty
        mappend (Reservation leftReservation)
          (Reservation rightReservation) = Reservation merged
          where merged = Map.unionWith merge leftReservation rightReservation
                merge left right = Monoid.mconcat [left, between, right]
                  where between
                          = [Abstract.EmptyLine |
                             isJustComment (ListTool.maybeLast left) &&
                               isJustComment (Maybe.listToMaybe right)]
                isJustComment maybeBox
                  = case maybeBox of
                        Nothing -> False
                        Just (Abstract.CommentBox _) -> True
                        Just Abstract.EmptyLine -> False

integrateComments :: Merged.Code -> Concrete.Commented
integrateComments merged
  = Concrete.createCommented movedCommentlessRoot comments
  where movedCommentlessRoot
          = Concrete.commentlessRoot movedCommentless
        movedCommentless = Shifting.shiftCode shifting commentless
        shifting = reservationShifting reservation
        reservation = makeReservation merged
        commentless = Merged.makeCommentless merged
        comments = createComments file reservation
        file = Location.fileName $ Location.getPortion movedCommentless

reservationShifting :: Reservation -> Shifting.LineShifting
reservationShifting
  = Shifting.createLineShifting . accumulateReservation create
  where create line _ shiftedShift _
          = Map.singleton line shiftedShift

accumulateReservation ::
                        (Monoid.Monoid m) =>
                        (Location.Line ->
                           Location.Line -> Shifting.LineShift -> [Abstract.Box] -> m)
                          -> Reservation -> m
accumulateReservation create (Reservation reservation)
  = snd $
      Map.foldlWithKey' accumulate (Monoid.mempty, Monoid.mempty)
        reservation
  where accumulate (shiftedShift, structure) line boxes
          = (shiftedShift', structure')
          where shiftedShift' = Monoid.mappend shiftedShift unshiftedShift
                unshiftedShift = boxesShift boxes
                structure' = Monoid.mappend structure part
                part = create line shiftedLine shiftedShift' boxes
                shiftedLine = Shifting.shiftLine shiftedShift line

boxesShift :: [Abstract.Box] -> Shifting.LineShift
boxesShift = Monoid.mconcat . fmap boxShift

boxShift :: Abstract.Box -> Shifting.LineShift
boxShift (Abstract.CommentBox comment)
  = commentShift $ Abstract.commentCore comment
  where commentShift
          = Shifting.LineShift . length . Comment.wrappedLines
boxShift Abstract.EmptyLine = Shifting.LineShift 1

makeReservation :: Merged.Code -> Reservation
makeReservation = Foldable.foldMap reservePart . Merged.codeRoot

reservePart :: Merged.Part -> Reservation
reservePart part = Reservation reservation
  where reservation = before `Map.union` after
        before
          = Map.singleton lineIfBefore $ Abstract.boxesBefore annotation
        lineIfBefore = Location.getStartLine portion
        portion = Location.getPortion part
        annotation = Merged.partAnnotation part
        after = Map.singleton lineIfAfter $ Abstract.boxesAfter annotation
        lineIfAfter = succ $ Location.getEndLine portion

createComments :: FilePath -> Reservation -> [Source.Comment]
createComments file = accumulateReservation create
  where create _ baseLine _ = snd . List.foldl' merge (baseLine, [])
        merge (startLine, concretePart) box
          = (followingLine, Monoid.mappend concretePart comments)
          where followingLine = Shifting.shiftLine shift startLine
                shift = boxShift box
                comments
                  = case box of
                        Abstract.CommentBox comment -> [createComment file startLine
                                                          comment]
                        Abstract.EmptyLine -> []

createComment ::
              FilePath -> Location.Line -> Abstract.Comment -> Source.Comment
createComment file startLine comment
  = Source.createComment core portion
  where core = Abstract.commentCore comment
        portion = Location.stringPortion startPosition wrappedComment
        startPosition = Location.createPosition file startLine startColumn
        startColumn = Abstract.commentStartColumn comment
        wrappedComment = show core
