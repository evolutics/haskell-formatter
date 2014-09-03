{-|
Description : Root of formatting
-}
module Language.Haskell.Formatter.Main (defaultFormat, format) where
import qualified Language.Haskell.Formatter.Configuration as Configuration
import qualified Language.Haskell.Formatter.Error as Error
import qualified Language.Haskell.Formatter.ExactCode as ExactCode
import qualified Language.Haskell.Formatter.Process.Control as Control
import qualified Language.Haskell.Formatter.Result as Result
import qualified Language.Haskell.Formatter.Source as Source
import qualified Language.Haskell.Formatter.Toolkit.StreamName as StreamName

defaultFormat :: String -> Either Error.Error String
defaultFormat = format Configuration.defaultConfiguration

format :: Configuration.Configuration -> String -> Either Error.Error String
format configuration = Result.toEither . tryFormat configuration

tryFormat :: Configuration.Configuration -> String -> Result.Result String
tryFormat configuration source
  = do Configuration.check configuration
       exact <- parse stream source
       exact' <- Control.format style exact
       return $ show exact'
  where stream = Configuration.configurationStreamName configuration
        style = Configuration.configurationStyle configuration

parse :: StreamName.StreamName -> String -> Result.Result ExactCode.ExactCode
parse stream source
  = case parseResult of
        Source.ParseFailed position message -> Result.fatalError parseError
          where parseError = Error.createParseError position message
        Source.ParseOk (actualCode, comments) -> return exact
          where exact = ExactCode.create actualCode comments
  where parseResult = Source.parseFileContentsWithComments mode source
        mode = Source.defaultParseMode{Source.parseFilename = show stream}
