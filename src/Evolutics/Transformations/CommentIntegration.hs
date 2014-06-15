module Evolutics.Transformations.CommentIntegration
       (integrateComments) where
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
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

data OriginLineShifting = OriginLineShifting (Map.Map LineIndex
                                                LineShift)

integrateComments ::
                  Abstract.Code -> Concrete.Commentless -> Concrete.Commented
integrateComments abstract commentless
  = Concrete.createCommented
      (Concrete.commentlessRoot movedCommentless)
      []
  where movedCommentless = shiftRoot shifting commentless
        shifting = lineShifting annotatedRoot
        annotatedRoot
          = AnnotatedRoot $
              Functions.halfZipWith ElementAnnotation abstractRoot
                commentlessRoot
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
        Nothing -> noLineShift

noLineShift :: LineShift
noLineShift = LineShift 0

startLine :: Exts.SrcSpan -> LineIndex
startLine = LineIndex . Exts.srcSpanStartLine

endLine :: Exts.SrcSpan -> LineIndex
endLine = LineIndex . Exts.srcSpanEndLine

lineShifting :: AnnotatedRoot -> LineShifting
lineShifting = accummulateLineShifting . originLineShifting

accummulateLineShifting :: OriginLineShifting -> LineShifting
accummulateLineShifting (OriginLineShifting shifting)
  = LineShifting . snd $
      Map.mapAccum accummulate noLineShift shifting
  where accummulate accummulatedShift
          = Functions.doubleArgument (,) . summarizeShifts accummulatedShift

originLineShifting :: AnnotatedRoot -> OriginLineShifting
originLineShifting (AnnotatedRoot root)
  = OriginLineShifting $ Foldable.foldl' process Map.empty root
  where process shifting annotation
          = Map.unionWith summarizeShifts shifting shiftingNow
          where OriginLineShifting shiftingNow = elementShifting annotation

summarizeShifts :: LineShift -> LineShift -> LineShift
summarizeShifts (LineShift left) (LineShift right)
  = LineShift $ left + right

elementShifting :: ElementAnnotation -> OriginLineShifting
elementShifting (ElementAnnotation comments location)
  = OriginLineShifting $ Foldable.foldl' process Map.empty comments
  where process shifting comment
          = Map.insertWith summarizeShifts shiftedLine difference shifting
          where shiftedLine
                  = case Abstract.commentDisplacement comment of
                        Abstract.Before -> lineIfBefore
                        Abstract.After -> lineIfAfter
                difference = LineShift $ Abstract.commentLineCount comment
        lineIfBefore = startLine portion
        portion = SourceLocations.portion location
        lineIfAfter = applyLineShift oneLineShift $ endLine portion

oneLineShift :: LineShift
oneLineShift = LineShift 1
