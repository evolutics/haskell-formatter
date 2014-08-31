{-|
Description : Haskell source code formatter
-}
module Language.Haskell.Formatter
       (Error.Error, Error.isAssertionError, Main.format,
        StreamName.createStreamName, StreamName.standardInput,
        StreamName.StreamName)
       where
import qualified Language.Haskell.Formatter.Error as Error
import qualified Language.Haskell.Formatter.Main as Main
import qualified Language.Haskell.Formatter.Toolkit.StreamName as StreamName
