{-|
Description : Errors for feedback to users
-}
module Language.Haskell.Formatter.Error
       (Error, createParseError, createAssertionError, isAssertionError) where
import qualified Language.Haskell.Formatter.Location as Location
import qualified Language.Haskell.Formatter.Source as Source

data Error = ParseError Location.SrcLoc String
           | AssertionError String
           deriving (Eq, Ord)

instance Show Error where
        show (ParseError position message)
          = concat [Source.prettyPrint position, separator, message]
          where separator = ": "
        show (AssertionError message) = message

createParseError :: Location.SrcLoc -> String -> Error
createParseError = ParseError

createAssertionError :: String -> Error
createAssertionError = AssertionError

isAssertionError :: Error -> Bool
isAssertionError (ParseError _ _) = False
isAssertionError (AssertionError _) = True
