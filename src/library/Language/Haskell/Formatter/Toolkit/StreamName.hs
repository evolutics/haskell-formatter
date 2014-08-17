{-|
Description : Naming data streams
-}
module Language.Haskell.Formatter.Toolkit.StreamName
       (StreamName, createStreamName, standardInput) where

{-| An informal reference to a data stream. For example, this could be the name
    of a file stream to be used in error messages. -}
data StreamName = StreamName String
                deriving (Eq, Ord)

instance Show StreamName where
        show (StreamName string) = string

{-| Creates a 'StreamName'. 'show' is guaranteed to return this string.

    prop> show (createStreamName s) == s -}
createStreamName :: String -> StreamName
createStreamName = StreamName

{-| The standard input stream (stdin). -}
standardInput :: StreamName
standardInput = createStreamName "<stdin>"
