{-|
Description : Haskell source code formatter
-}
module Language.Haskell.Formatter
       (Configuration.Configuration(..), Configuration.defaultConfiguration,
        Error.Error, Error.isAssertionError, Main.defaultFormat, Main.format,
        StreamName.createStreamName, StreamName.standardInput,
        StreamName.StreamName, Style.Style(..))
       where
import qualified Language.Haskell.Formatter.Configuration as Configuration
import qualified Language.Haskell.Formatter.Error as Error
import qualified Language.Haskell.Formatter.Main as Main
import qualified Language.Haskell.Formatter.Style as Style
import qualified Language.Haskell.Formatter.Toolkit.StreamName as StreamName
