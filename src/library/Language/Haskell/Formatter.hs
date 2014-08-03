module Language.Haskell.Formatter
       (Error.Error, Formatting.format, StreamName.createStreamName,
        StreamName.standardInput)
       where
import qualified Language.Haskell.Formatter.Error as Error
import qualified Language.Haskell.Formatter.Formatting
       as Formatting
import qualified Language.Haskell.Formatter.Toolkit.StreamName
       as StreamName
