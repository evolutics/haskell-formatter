module Evolutics.Code.Helper (formatMessage, comparePortions) where
import qualified Evolutics.Code.Location as Location
import qualified Evolutics.Code.Source as Source

formatMessage :: Location.SrcLoc -> String -> String
formatMessage position message
  = Source.prettyPrint position ++ separator ++ message
  where separator = ": "

comparePortions :: Location.SrcSpan -> Location.SrcSpan -> Ordering
comparePortions left right
  | Location.getEndPosition left < Location.getPointLoc right = LT
  | Location.getPointLoc left > Location.getEndPosition right = GT
  | otherwise = EQ
