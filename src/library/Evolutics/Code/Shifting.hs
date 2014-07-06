module Evolutics.Code.Shifting
       (LineShifting, LineShift(..), createLineShifting, shiftCode,
        shiftLine)
       where
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid
import qualified Evolutics.Code.Concrete as Concrete
import qualified Evolutics.Code.Location as Location

data LineShifting = LineShifting (Map.Map Location.Line LineShift)
                  deriving (Eq, Ord, Show)

data LineShift = LineShift Int
               deriving (Eq, Ord, Show)

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
  = commentless{Concrete.commentlessRoot = shiftedRoot}
  where shiftedRoot
          = fmap (shiftNestedPortion shifting) unshiftedRoot
        unshiftedRoot = Concrete.commentlessRoot commentless

shiftNestedPortion ::
                   LineShifting -> Location.SrcSpanInfo -> Location.SrcSpanInfo
shiftNestedPortion = Location.mapNestedPortion . applyLineShifting

applyLineShifting :: LineShifting -> Location.Line -> Location.Line
applyLineShifting shifting line = shiftLine shift line
  where shift = lookupLineShift shifting line

shiftLine :: LineShift -> Location.Line -> Location.Line
shiftLine (LineShift shift) line = Location.add shift line

lookupLineShift :: LineShifting -> Location.Line -> LineShift
lookupLineShift (LineShifting shifting) line
  = case Map.lookupLE line shifting of
        Just (_, shift) -> shift
        Nothing -> Monoid.mempty
