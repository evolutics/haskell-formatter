{-|
Description : Working with lines of code
-}
module Language.Haskell.Formatter.Process.LineTool
       (Shifter, Shift, countEmptyLines, createShifter, shiftCode) where
import qualified Data.Map.Strict as Map
import qualified Language.Haskell.Formatter.Location as Location
import qualified Language.Haskell.Formatter.Process.Code as Code

newtype Shifter = Shifter (Map.Map Location.Line Shift)
                    deriving (Eq, Ord, Show)

type Shift = Int

countEmptyLines :: Location.Line -> Location.Line -> Int
countEmptyLines endLine startLine = pred lineDifference
  where lineDifference = Location.minus startLine endLine

createShifter :: Map.Map Location.Line Shift -> Shifter
createShifter relativeShifter = Shifter absoluteShifter
  where (_, absoluteShifter) = Map.mapAccum accumulate noShift relativeShifter
        accumulate absoluteShift relativeShift
          = (absoluteShift', absoluteShift')
          where absoluteShift' = absoluteShift + relativeShift

noShift :: Shift
noShift = 0

shiftCode :: Shifter -> Code.LocatableCode -> Code.LocatableCode
shiftCode shifter = fmap $ shiftNestedPortion shifter
  where shiftNestedPortion = Location.replaceNestedPortionLines . shiftLine

shiftLine :: Shifter -> Location.Line -> Location.Line
shiftLine shifter line = Location.plus shift line
  where shift = lookupShift line shifter

lookupShift :: Location.Line -> Shifter -> Shift
lookupShift line (Shifter shifter)
  = case Map.lookupLE line shifter of
        Nothing -> noShift
        Just (_, shift) -> shift
