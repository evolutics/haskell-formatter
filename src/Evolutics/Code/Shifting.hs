module Evolutics.Code.Shifting
       (LineShifting, LineShift(..), createLineShifting, shiftCode,
        shiftLine)
       where
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid
import qualified Language.Haskell.Exts.Annotated as Exts
import qualified Evolutics.Code.Concrete as Concrete
import qualified Evolutics.Code.Location as Location

data LineShifting = LineShifting (Map.Map Location.Line LineShift)

data LineShift = LineShift Int

instance Monoid.Monoid LineShift where
        mempty = LineShift 0
        mappend (LineShift left) (LineShift right)
          = LineShift $ left + right

createLineShifting ::
                   Map.Map Location.Line LineShift -> LineShifting
createLineShifting = LineShifting

shiftCode ::
          LineShifting -> Concrete.Commentless -> Concrete.Commentless
shiftCode shifting commentless
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
  where Location.Line shiftedStart = shift originalStart
        shift = applyLineShifting shifting
        originalStart = Location.startLine portion
        Location.Line shiftedEnd = shift originalEnd
        originalEnd = Location.endLine portion

applyLineShifting :: LineShifting -> Location.Line -> Location.Line
applyLineShifting shifting line = shiftLine shift line
  where shift = lookupLineShift shifting line

shiftLine :: LineShift -> Location.Line -> Location.Line
shiftLine (LineShift shift) (Location.Line line)
  = Location.Line $ line + shift

lookupLineShift :: LineShifting -> Location.Line -> LineShift
lookupLineShift (LineShifting shifting) line
  = case Map.lookupLE line shifting of
        Just (_, shift) -> shift
        Nothing -> Monoid.mempty
