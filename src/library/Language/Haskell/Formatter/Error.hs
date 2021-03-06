{-|
Description : Errors for feedback to users
-}
module Language.Haskell.Formatter.Error
       (Error, createStyleFormatError, createParseError, createAssertionError,
        isAssertionError)
       where
import qualified Language.Haskell.Formatter.Location as Location
import qualified Language.Haskell.Formatter.Source as Source

data Error = StyleFormatError String
           | ParseError Location.SrcLoc String
           | AssertionError String
               deriving (Eq, Ord)

instance Show Error where
        show (StyleFormatError message) = message
        show (ParseError position message)
          = concat [Source.prettyPrint position, separator, message]
          where separator = ": "
        show (AssertionError message) = message

createStyleFormatError :: String -> Error
createStyleFormatError = StyleFormatError

createParseError :: Location.SrcLoc -> String -> Error
createParseError = ParseError

createAssertionError :: String -> Error
createAssertionError = AssertionError

isAssertionError :: Error -> Bool
isAssertionError (StyleFormatError _) = False
isAssertionError (ParseError _ _) = False
isAssertionError (AssertionError _) = True
