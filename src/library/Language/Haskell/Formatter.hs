{-|
Description : Haskell source code formatter
-}
module Language.Haskell.Formatter
       (module Configuration, Error.Error, Error.isAssertionError,
        Main.defaultFormat, Main.format, StreamName.createStreamName,
        StreamName.standardInput, StreamName.StreamName, module Style)
       where
import Language.Haskell.Formatter.Configuration as Configuration hiding (check)
import qualified Language.Haskell.Formatter.Error as Error
import qualified Language.Haskell.Formatter.Main as Main
import Language.Haskell.Formatter.Style as Style hiding (check)
import qualified Language.Haskell.Formatter.Toolkit.StreamName as StreamName
