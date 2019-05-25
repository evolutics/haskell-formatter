{-|
Description : Overall configuration
-}
module Language.Haskell.Formatter.Configuration
       (Configuration, configurationStyle, configurationStreamName,
        defaultConfiguration, check)
       where
import qualified Language.Haskell.Formatter.Result as Result
import qualified Language.Haskell.Formatter.Source as Source
import qualified Language.Haskell.Formatter.Style as Style
import qualified Language.Haskell.Formatter.Toolkit.StreamName as StreamName

data Configuration = Configuration{configurationStyle :: Style.Style,
                                   configurationStreamName ::
                                   StreamName.StreamName}
                       deriving (Eq, Ord, Show)

defaultConfiguration :: Configuration
defaultConfiguration
  = Configuration{configurationStyle = Style.defaultStyle,
                  configurationStreamName = stream}
  where stream = StreamName.createStreamName filename
        filename = Source.parseFilename Source.defaultParseMode

check :: Configuration -> Result.Result ()
check = Style.check . configurationStyle
