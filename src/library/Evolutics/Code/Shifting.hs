module Evolutics.Code.Shifting
       (LineShifting, LineShift(..), createLineShifting, shiftCode,
        shiftLine)
       where
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid
import qualified Evolutics.Code.Concrete as Concrete
import qualified Evolutics.Code.Source as Source

data LineShifting = LineShifting (Map.Map Source.Line LineShift)

data LineShift = LineShift Int

instance Monoid.Monoid LineShift where
        mempty = LineShift 0
        mappend (LineShift left) (LineShift right)
          = LineShift $ left + right

createLineShifting :: Map.Map Source.Line LineShift -> LineShifting
createLineShifting = LineShifting

shiftCode ::
          LineShifting -> Concrete.Commentless -> Concrete.Commentless
shiftCode shifting commentless
  = commentless{Concrete.commentlessRoot = shiftedRoot}
  where shiftedRoot
          = fmap (shiftNestedPortion shifting) unshiftedRoot
        unshiftedRoot = Concrete.commentlessRoot commentless

shiftNestedPortion ::
                   LineShifting -> Source.SrcSpanInfo -> Source.SrcSpanInfo
shiftNestedPortion = Source.mapNestedPortion . applyLineShifting

applyLineShifting :: LineShifting -> Source.Line -> Source.Line
applyLineShifting shifting line = shiftLine shift line
  where shift = lookupLineShift shifting line

shiftLine :: LineShift -> Source.Line -> Source.Line
shiftLine (LineShift shift) line = Source.add shift line

lookupLineShift :: LineShifting -> Source.Line -> LineShift
lookupLineShift (LineShifting shifting) line
  = case Map.lookupLE line shifting of
        Just (_, shift) -> shift
        Nothing -> Monoid.mempty
