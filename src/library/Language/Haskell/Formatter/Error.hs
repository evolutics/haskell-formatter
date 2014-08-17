module Language.Haskell.Formatter.Error (Error(..)) where
import qualified Data.Monoid as Monoid
import qualified Language.Haskell.Formatter.Code.Location
       as Location
import qualified Language.Haskell.Formatter.Code.Source as Source

data Error = ParseError Location.SrcLoc String
           | AssertionError String
           deriving (Eq, Ord)

instance Show Error where
        show (ParseError position message)
          = Monoid.mconcat [Source.prettyPrint position, separator, message]
          where separator = ": "
        show (AssertionError message) = message
