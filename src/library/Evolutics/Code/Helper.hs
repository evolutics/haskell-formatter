module Evolutics.Code.Helper (formatMessage) where
import qualified Data.Monoid as Monoid
import qualified Evolutics.Code.Location as Location
import qualified Evolutics.Code.Source as Source

formatMessage :: Location.SrcLoc -> String -> String
formatMessage position message
  = Monoid.mconcat [Source.prettyPrint position, separator, message]
  where separator = ": "
