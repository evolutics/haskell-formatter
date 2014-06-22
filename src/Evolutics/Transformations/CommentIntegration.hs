module Evolutics.Transformations.CommentIntegration
       (integrateComments) where
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid
import qualified Evolutics.Code.Abstract as Abstract
import qualified Evolutics.Code.Comment as Comment
import qualified Evolutics.Code.Concrete as Concrete
import qualified Evolutics.Code.Core as Core
import qualified Evolutics.Code.Locations as Locations
import qualified Evolutics.Code.Shifting as Shifting
import qualified Evolutics.Tools.Functions as Functions

data MergedCode = MergedCode (Core.Module MergedAnnotation)

data MergedAnnotation = MergedAnnotation Abstract.Annotation
                                         Core.SrcSpan

data Reservation = Reservation (Map.Map Locations.Line
                                  [Abstract.Comment])

integrateComments ::
                  Abstract.Code -> Concrete.Commentless -> Concrete.Commented
integrateComments abstract commentless
  = Concrete.createCommented movedCommentlessRoot comments
  where movedCommentlessRoot
          = Concrete.commentlessRoot movedCommentless
        movedCommentless = Shifting.shiftCode shifting commentless
        shifting = reservationShifting reservation
        reservation = makeReservation mergedRoot
        mergedRoot = mergeParts abstract commentless
        comments = concretizeComments file reservation
        file = Core.fileName $ Locations.portion movedCommentless

reservationShifting :: Reservation -> Shifting.LineShifting
reservationShifting
  = Shifting.createLineShifting . accummulateReservation create
  where create line _ shiftedShift _
          = Map.singleton line shiftedShift

accummulateReservation ::
                         (Monoid.Monoid m) =>
                         (Locations.Line ->
                            Locations.Line -> Shifting.LineShift -> [Abstract.Comment] -> m)
                           -> Reservation -> m
accummulateReservation create (Reservation reservation)
  = snd $
      Map.foldlWithKey' accummulate (Monoid.mempty, Monoid.mempty)
        reservation
  where accummulate (shiftedShift, structure) line comments
          = (shiftedShift', structure')
          where shiftedShift' = Monoid.mappend shiftedShift unshiftedShift
                unshiftedShift = commentsShift comments
                structure' = Monoid.mappend structure part
                part = create line shiftedLine shiftedShift' comments
                shiftedLine = Shifting.shiftLine shiftedShift line

commentsShift :: [Abstract.Comment] -> Shifting.LineShift
commentsShift
  = Monoid.mconcat . map (commentShift . Abstract.commentCore)

commentShift :: Comment.Comment -> Shifting.LineShift
commentShift = Shifting.LineShift . length . Comment.wrappedLines

makeReservation :: MergedCode -> Reservation
makeReservation (MergedCode root)
  = Reservation $ Foldable.foldl' reserve Map.empty root
  where reserve reservation mergedAnnotation
          = Map.unionWith mergeReservations reservation reservationNow
          where Reservation reservationNow
                  = annotationReservation mergedAnnotation

mergeReservations ::
                  [Abstract.Comment] -> [Abstract.Comment] -> [Abstract.Comment]
mergeReservations = (++)

annotationReservation :: MergedAnnotation -> Reservation
annotationReservation (MergedAnnotation annotation portion)
  = Reservation reservation
  where reservation = before `Map.union` after
        before
          = Map.singleton lineIfBefore $ Abstract.commentsBefore annotation
        lineIfBefore = Locations.startLine portion
        after
          = Map.singleton lineIfAfter $ Abstract.commentsAfter annotation
        lineIfAfter = Locations.successorLine $ Locations.endLine portion

mergeParts :: Abstract.Code -> Concrete.Commentless -> MergedCode
mergeParts abstract commentless = MergedCode merged
  where merged
          = if abstractRoot Core.=~= commentlessRoot then
              Functions.halfZipWith merge abstractRoot commentlessRoot else
              error unequalStructuresMessage
        abstractRoot = Abstract.codeRoot abstract
        commentlessRoot = Concrete.commentlessRoot commentless
        merge annotation = MergedAnnotation annotation . Locations.portion

unequalStructuresMessage :: String
unequalStructuresMessage
  = "The structures of the abstract and concrete code are unequal."

concretizeComments :: FilePath -> Reservation -> [Core.Comment]
concretizeComments file = accummulateReservation create
  where create _ baseLine _ = snd . List.foldl' merge (baseLine, [])
        merge (startLine, concretePart) comment
          = (followingLine, concretePart ++ [concrete])
          where followingLine = Shifting.shiftLine shift startLine
                shift = commentShift core
                core = Abstract.commentCore comment
                concrete = Concrete.createComment core startPosition
                startPosition = Locations.createPosition file startLine startColumn
                startColumn = Abstract.commentStartColumn comment
