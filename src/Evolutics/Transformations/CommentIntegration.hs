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

integrateComments ::
                  Abstract.Code -> Concrete.Commentless -> Concrete.Commented
integrateComments abstract concreteCommentless
  = Concrete.createCommented
      (Concrete.commentlessRoot concreteCommentless)
      []
  where abstractRoot = Abstract.codeRoot abstract
        concreteCommentlessRoot
          = Concrete.commentlessRoot concreteCommentless
        annotatedRoot
          = AnnotatedRoot $
              Functions.halfZipWith ElementAnnotation abstractRoot
                concreteCommentlessRoot

lineShifting :: AnnotatedRoot -> LineShifting
lineShifting root
  = LineShifting . snd $
      Map.mapAccum accummulate noShift originShifting
  where accummulate accummulatedShift
          = Functions.doubleArgument (,) . summarizeShifts accummulatedShift
        noShift = LineShift 0
        LineShifting originShifting = originLineShifting root

originLineShifting :: AnnotatedRoot -> LineShifting
originLineShifting (AnnotatedRoot root)
  = LineShifting $ Foldable.foldl' process Map.empty root
  where process shifting annotation
          = Map.unionWith summarizeShifts shifting shiftingNow
          where LineShifting shiftingNow = elementShifting annotation

summarizeShifts :: LineShift -> LineShift -> LineShift
summarizeShifts (LineShift left) (LineShift right)
  = LineShift $ left + right

elementShifting :: ElementAnnotation -> LineShifting
elementShifting (ElementAnnotation comments location)
  = LineShifting $ Foldable.foldl' process Map.empty comments
  where process shifting comment
          = Map.insertWith summarizeShifts shiftedLine difference shifting
          where shiftedLine
                  = case Abstract.commentDisplacement comment of
                        Abstract.Before -> lineIfBefore
                        Abstract.After -> lineIfAfter
                difference = LineShift $ Abstract.commentLineCount comment
        lineIfBefore = LineIndex $ Exts.srcSpanStartLine portion
        portion = SourceLocations.portion location
        lineIfAfter = LineIndex $ Exts.srcSpanEndLine portion + 1
