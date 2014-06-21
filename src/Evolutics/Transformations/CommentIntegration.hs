module Evolutics.Transformations.CommentIntegration
       (integrateComments) where
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid
import qualified Evolutics.Code.Abstract as Abstract
import qualified Evolutics.Code.Concrete as Concrete
import qualified Evolutics.Code.Core as Core
import qualified Evolutics.Code.Locations as Locations
import qualified Evolutics.Code.Shifting as Shifting
import qualified Evolutics.Tools.Functions as Functions

data AnnotatedRoot = AnnotatedRoot (Core.Module ElementAnnotation)

data ElementAnnotation = ElementAnnotation [Abstract.Comment]
                                           Core.SrcSpanInfo

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
        reservation = makeReservation annotatedRoot
        annotatedRoot
          = AnnotatedRoot $
              if abstractRoot Core.=~= commentlessRoot then
                Functions.halfZipWith ElementAnnotation abstractRoot
                  commentlessRoot
                else error unequalStructuresMessage
        abstractRoot = Abstract.codeRoot abstract
        commentlessRoot = Concrete.commentlessRoot commentless
        comments = concretizeComments file reservation
        file = Core.fileName $ Locations.portion movedCommentless

reservationShifting :: Reservation -> Shifting.LineShifting
reservationShifting
  = Shifting.createLineShifting . accummulateReservation create
  where create line _ shift _ = Map.singleton line shift

accummulateReservation ::
                         (Monoid.Monoid m) =>
                         (Locations.Line ->
                            Locations.Line -> Shifting.LineShift -> [Abstract.Comment] -> m)
                           -> Reservation -> m
accummulateReservation create (Reservation reservation)
  = snd $
      Map.foldlWithKey' accummulate (Monoid.mempty, Monoid.mempty)
        reservation
  where accummulate (shift, structure) line comments
          = (shift', structure')
          where shift' = Monoid.mappend shift $ commentsShift comments
                structure' = Monoid.mappend structure part
                part = create line shiftedLine shift' comments
                shiftedLine = Shifting.shiftLine shift line

commentsShift :: [Abstract.Comment] -> Shifting.LineShift
commentsShift = Monoid.mconcat . map commentShift

commentShift :: Abstract.Comment -> Shifting.LineShift
commentShift = Shifting.LineShift . Abstract.commentLineCount

makeReservation :: AnnotatedRoot -> Reservation
makeReservation (AnnotatedRoot root)
  = Reservation $ Foldable.foldl' reserve Map.empty root
  where reserve reservation annotation
          = Map.unionWith mergeReservations reservation reservationNow
          where Reservation reservationNow = elementReservation annotation

mergeReservations ::
                  [Abstract.Comment] -> [Abstract.Comment] -> [Abstract.Comment]
mergeReservations = (++)

elementReservation :: ElementAnnotation -> Reservation
elementReservation (ElementAnnotation comments location)
  = Reservation $ foldr reserve Map.empty comments
  where reserve comment
          = Map.insertWith mergeReservations movedLine [comment]
          where movedLine
                  = case Abstract.commentDisplacement comment of
                        Abstract.Before -> lineIfBefore
                        Abstract.After -> lineIfAfter
        lineIfBefore = Locations.startLine portion
        portion = Locations.portion location
        lineIfAfter = Locations.successorLine $ Locations.endLine portion

unequalStructuresMessage :: String
unequalStructuresMessage
  = "The structures of the abstract and concrete code are unequal."

concretizeComments :: FilePath -> Reservation -> [Core.Comment]
concretizeComments file = accummulateReservation create
  where create _ shiftedLine _
          = snd . Foldable.foldl' merge (shiftedLine, [])
        merge (startLine, concretePart) abstract
          = (followingLine, concretePart ++ [concrete])
          where followingLine = Shifting.shiftLine shift startLine
                shift = commentShift abstract
                concrete = Concrete.createComment kind content startPosition
                kind = Abstract.commentKind abstract
                content = Abstract.commentContent abstract
                startPosition
                  = Core.SrcLoc{Core.srcFilename = file, Core.srcLine = rawStartLine,
                                Core.srcColumn = startColumn}
                Locations.Line rawStartLine = startLine
                startColumn = 1
