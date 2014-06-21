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
elementReservation (ElementAnnotation comments nestedPortion)
  = Reservation $ foldr reserve Map.empty comments
  where reserve comment
          = Map.insertWith mergeReservations movedLine [comment]
          where movedLine
                  = case Abstract.commentDisplacement comment of
                        Abstract.Before -> lineIfBefore
                        Abstract.After -> lineIfAfter
        lineIfBefore = Locations.startLine portion
        portion = Locations.portion nestedPortion
        lineIfAfter = Locations.successorLine $ Locations.endLine portion

unequalStructuresMessage :: String
unequalStructuresMessage
  = "The structures of the abstract and concrete code are unequal."

concretizeComments :: FilePath -> Reservation -> [Core.Comment]
concretizeComments file = accummulateReservation create
  where create _ baseLineBefore _ abstractComments
          = concreteBefore ++ concreteAfter
          where (originLine, concreteBefore) = run baseLineBefore coresBefore
                run = concretizeRun file
                (coresBefore, coresAfter) = unwrapComments abstractComments
                (_, concreteAfter) = run baseLineAfter coresAfter
                baseLineAfter = Locations.successorLine originLine

unwrapComments ::
               [Abstract.Comment] -> ([Comment.Comment], [Comment.Comment])
unwrapComments = foldr unwrap ([], [])
  where unwrap comment (before, after)
          = case Abstract.commentDisplacement comment of
                Abstract.Before -> (core : before, after)
                Abstract.After -> (before, core : after)
          where core = Abstract.commentCore comment

concretizeRun ::
              FilePath ->
                Locations.Line ->
                  [Comment.Comment] -> (Locations.Line, [Core.Comment])
concretizeRun file baseLine = List.foldl' merge (baseLine, [])
  where merge (startLine, concretePart) core
          = (followingLine, concretePart ++ [concrete])
          where followingLine = Shifting.shiftLine shift startLine
                shift = commentShift core
                concrete = Concrete.createComment core startPosition
                startPosition
                  = Core.SrcLoc{Core.srcFilename = file, Core.srcLine = rawStartLine,
                                Core.srcColumn = startColumn}
                Locations.Line rawStartLine = startLine
                startColumn = 1
