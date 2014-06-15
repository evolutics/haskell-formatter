module Evolutics.Transformations.CommentIntegration
       (integrateComments) where
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid
import qualified Language.Haskell.Exts.Annotated as Exts
import qualified Evolutics.Code.Abstract as Abstract
import qualified Evolutics.Code.Concrete as Concrete
import qualified Evolutics.Tools.Functions as Functions
import qualified Evolutics.Tools.SourceLocations as SourceLocations

data AnnotatedRoot = AnnotatedRoot (Exts.Module ElementAnnotation)

data ElementAnnotation = ElementAnnotation [Abstract.Comment]
                                           Exts.SrcSpanInfo

data LineShifting = LineShifting (Map.Map LineIndex LineShift)

data LineIndex = LineIndex Int
               deriving (Eq, Ord)

data LineShift = LineShift Int

data Reservation = Reservation (Map.Map LineIndex
                                  [Abstract.Comment])

instance Monoid.Monoid LineShift where
        mempty = LineShift 0
        mappend (LineShift left) (LineShift right)
          = LineShift $ left + right

integrateComments ::
                  Abstract.Code -> Concrete.Commentless -> Concrete.Commented
integrateComments abstract commentless
  = Concrete.createCommented
      (Concrete.commentlessRoot movedCommentless)
      []
  where movedCommentless = shiftRoot shifting commentless
        shifting = reservationShifting reservation
        reservation = makeReservation annotatedRoot
        annotatedRoot
          = AnnotatedRoot $
              if abstractRoot Exts.=~= commentlessRoot then
                Functions.halfZipWith ElementAnnotation abstractRoot
                  commentlessRoot
                else error unequalStructuresMessage
        abstractRoot = Abstract.codeRoot abstract
        commentlessRoot = Concrete.commentlessRoot commentless

shiftRoot ::
          LineShifting -> Concrete.Commentless -> Concrete.Commentless
shiftRoot shifting commentless
  = Concrete.createCommentless shiftedRoot
  where shiftedRoot = fmap (shiftLocation shifting) unshiftedRoot
        unshiftedRoot = Concrete.commentlessRoot commentless

shiftLocation ::
              LineShifting -> Exts.SrcSpanInfo -> Exts.SrcSpanInfo
shiftLocation shifting location
  = location{Exts.srcInfoSpan = shiftedParent,
             Exts.srcInfoPoints = shiftedChildren}
  where shiftedParent = shift originalParent
        shift = shiftPortion shifting
        originalParent = Exts.srcInfoSpan location
        shiftedChildren = map shift originalChildren
        originalChildren = Exts.srcInfoPoints location

shiftPortion :: LineShifting -> Exts.SrcSpan -> Exts.SrcSpan
shiftPortion shifting portion
  = portion{Exts.srcSpanStartLine = shiftedStart,
            Exts.srcSpanEndLine = shiftedEnd}
  where LineIndex shiftedStart = shiftLine originalStart
        shiftLine = applyLineShifting shifting
        originalStart = startLine portion
        LineIndex shiftedEnd = shiftLine originalEnd
        originalEnd = endLine portion

applyLineShifting :: LineShifting -> LineIndex -> LineIndex
applyLineShifting shifting line = applyLineShift shift line
  where shift = lookupLineShift shifting line

applyLineShift :: LineShift -> LineIndex -> LineIndex
applyLineShift (LineShift shift) (LineIndex line)
  = LineIndex $ line + shift

lookupLineShift :: LineShifting -> LineIndex -> LineShift
lookupLineShift (LineShifting shifting) line
  = case Map.lookupLE line shifting of
        Just (_, shift) -> shift
        Nothing -> Monoid.mempty

startLine :: Exts.SrcSpan -> LineIndex
startLine = LineIndex . Exts.srcSpanStartLine

endLine :: Exts.SrcSpan -> LineIndex
endLine = LineIndex . Exts.srcSpanEndLine

reservationShifting :: Reservation -> LineShifting
reservationShifting = LineShifting . accummulateReservation create
  where create line _ shift _ = Map.singleton line shift

accummulateReservation ::
                         (Monoid.Monoid m) =>
                         (LineIndex -> LineIndex -> LineShift -> [Abstract.Comment] -> m) ->
                           Reservation -> m
accummulateReservation create (Reservation reservation)
  = snd $
      Map.foldlWithKey' accummulate (Monoid.mempty, Monoid.mempty)
        reservation
  where accummulate (shift, structure) line comments
          = (shift', structure')
          where shift' = Monoid.mappend shift $ commentsShift comments
                structure' = Monoid.mappend structure part
                part = create line shiftedLine shift' comments
                shiftedLine = applyLineShift shift line

commentsShift :: [Abstract.Comment] -> LineShift
commentsShift = Monoid.mconcat . map commentShift

commentShift :: Abstract.Comment -> LineShift
commentShift = LineShift . Abstract.commentLineCount

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
        lineIfBefore = startLine portion
        portion = SourceLocations.portion location
        lineIfAfter = applyLineShift oneLineShift $ endLine portion

oneLineShift :: LineShift
oneLineShift = LineShift 1

unequalStructuresMessage :: String
unequalStructuresMessage
  = "The structures of the abstract and concrete code are unequal."
