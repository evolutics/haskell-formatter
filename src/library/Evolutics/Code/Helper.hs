module Evolutics.Code.Helper (formatMessage) where
import qualified Evolutics.Code.Location as Location
import qualified Evolutics.Code.Source as Source

formatMessage :: Location.SrcLoc -> String -> String
formatMessage position message
  = Source.prettyPrint position ++ separator ++ message
  where separator = ": "
