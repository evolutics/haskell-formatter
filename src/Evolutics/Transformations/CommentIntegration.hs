module Evolutics.Transformations.CommentIntegration
       (integrateComments) where
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Language.Haskell.Exts.Annotated as Exts
import qualified Evolutics.Code.Abstract as Abstract
import qualified Evolutics.Code.Concrete as Concrete
import qualified Evolutics.Tools.Functions as Functions
import qualified Evolutics.Tools.SourceLocations as SourceLocations

type LineIndex = Int

integrateComments ::
                  Abstract.Code -> Concrete.Commentless -> Concrete.Commented
integrateComments abstract concreteCommentless
  = Concrete.createCommented
      (Concrete.commentlessRoot concreteCommentless)
      []
  where abstractRoot = Abstract.codeRoot abstract
        concreteCommentlessRoot
          = Concrete.commentlessRoot concreteCommentless
        zippedRoot
          = Functions.halfZipWith (,) abstractRoot concreteCommentlessRoot

lineShifts ::
           Exts.Module ([Abstract.Comment], Exts.SrcSpanInfo) ->
             Map.Map LineIndex Int
lineShifts root
  = snd $ Map.mapAccum accummulate baseShift originShifts
  where accummulate accummulatedShift
          = Functions.doubleArgument (,) . (accummulatedShift +)
        baseShift = 0
        originShifts = originLineShifts root

originLineShifts ::
                 Exts.Module ([Abstract.Comment], Exts.SrcSpanInfo) ->
                   Map.Map LineIndex Int
originLineShifts = Foldable.foldl' process Map.empty
  where process shifts (comments, location)
          = Map.unionWith (+) shifts shiftsNow
          where shiftsNow
                  = elementShifts comments $ SourceLocations.portion location

elementShifts ::
              [Abstract.Comment] -> Exts.SrcSpan -> Map.Map LineIndex Int
elementShifts comments portion
  = Foldable.foldl' process Map.empty comments
  where process shifts comment
          = Map.insertWith (+) line difference shifts
          where line
                  = case Abstract.commentDisplacement comment of
                        Abstract.Before -> startLine
                        Abstract.After -> endLine + 1
                difference = Abstract.commentLineCount comment
        startLine = Exts.srcSpanStartLine portion
        endLine = Exts.srcSpanEndLine portion
