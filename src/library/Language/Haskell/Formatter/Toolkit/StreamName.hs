module Language.Haskell.Formatter.Toolkit.StreamName
       (StreamName, createStreamName, standardInput) where

data StreamName = StreamName String
                deriving (Eq, Ord)

instance Show StreamName where
        show (StreamName string) = string

createStreamName :: String -> StreamName
createStreamName = StreamName

standardInput :: StreamName
standardInput = createStreamName "<stdin>"
