{-|
Description : Root of formatting
-}
module Language.Haskell.Formatter.Main (format) where
import qualified Language.Haskell.Formatter.Error as Error
import qualified Language.Haskell.Formatter.ExactCode as ExactCode
import qualified Language.Haskell.Formatter.Process.Control as Control
import qualified Language.Haskell.Formatter.Result as Result
import qualified Language.Haskell.Formatter.Source as Source
import qualified Language.Haskell.Formatter.Toolkit.StreamName as StreamName

format :: StreamName.StreamName -> String -> Either Error.Error String
format stream = Result.toEither . tryFormat stream

tryFormat :: StreamName.StreamName -> String -> Result.Result String
tryFormat stream source
  = do exact <- parse stream source
       exact' <- Control.format exact
       return $ show exact'

parse :: StreamName.StreamName -> String -> Result.Result ExactCode.ExactCode
parse stream source
  = case parseResult of
        Source.ParseFailed position message -> Result.fatalError parseError
          where parseError = Error.createParseError position message
        Source.ParseOk (actualCode, comments) -> return exact
          where exact = ExactCode.create actualCode comments
  where parseResult = Source.parseFileContentsWithComments mode source
        mode = Source.defaultParseMode{Source.parseFilename = show stream}
