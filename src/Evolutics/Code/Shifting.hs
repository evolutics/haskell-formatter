module Evolutics.Code.Shifting
       (LineShifting, LineShift(..), createLineShifting, shiftCode,
        shiftLine)
       where
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid
import qualified Evolutics.Code.Concrete as Concrete
import qualified Evolutics.Code.Core as Core
import qualified Evolutics.Code.Locations as Locations

data LineShifting = LineShifting (Map.Map Locations.Line LineShift)

data LineShift = LineShift Int

instance Monoid.Monoid LineShift where
        mempty = LineShift 0
        mappend (LineShift left) (LineShift right)
          = LineShift $ left + right

createLineShifting ::
                   Map.Map Locations.Line LineShift -> LineShifting
createLineShifting = LineShifting

shiftCode ::
          LineShifting -> Concrete.Commentless -> Concrete.Commentless
shiftCode shifting commentless
  = commentless{Concrete.commentlessRoot = shiftedRoot}
  where shiftedRoot
          = fmap (shiftNestedPortion shifting) unshiftedRoot
        unshiftedRoot = Concrete.commentlessRoot commentless

shiftNestedPortion ::
                   LineShifting -> Core.SrcSpanInfo -> Core.SrcSpanInfo
shiftNestedPortion = Locations.mapPortions . shiftPortion

shiftPortion :: LineShifting -> Core.SrcSpan -> Core.SrcSpan
shiftPortion shifting portion
  = portion{Core.srcSpanStartLine = start',
            Core.srcSpanEndLine = end'}
  where Locations.Line start' = shift start
        shift = applyLineShifting shifting
        start = Locations.startLine portion
        Locations.Line end' = shift end
        end = Locations.endLine portion

applyLineShifting ::
                  LineShifting -> Locations.Line -> Locations.Line
applyLineShifting shifting line = shiftLine shift line
  where shift = lookupLineShift shifting line

shiftLine :: LineShift -> Locations.Line -> Locations.Line
shiftLine (LineShift shift) (Locations.Line line)
  = Locations.Line $ line + shift

lookupLineShift :: LineShifting -> Locations.Line -> LineShift
lookupLineShift (LineShifting shifting) line
  = case Map.lookupLE line shifting of
        Just (_, shift) -> shift
        Nothing -> Monoid.mempty
