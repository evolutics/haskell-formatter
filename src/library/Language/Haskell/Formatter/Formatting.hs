module Language.Haskell.Formatter.Formatting (format) where
import qualified Language.Haskell.Formatter.Code.Concrete
       as Concrete
import qualified Language.Haskell.Formatter.Code.Source as Source
import qualified Language.Haskell.Formatter.Default.Formatter
       as Default
import qualified Language.Haskell.Formatter.Error as Error
import qualified Language.Haskell.Formatter.Formatter as Formatter
import qualified Language.Haskell.Formatter.Result as Result

format :: Maybe FilePath -> String -> Either Error.Error String
format maybeFile = Result.toEither . tryFormat maybeFile

tryFormat :: Maybe FilePath -> String -> Result.Result String
tryFormat maybeFile source
  = do code <- parse maybeFile source
       code' <- Formatter.formatCode formatter code
       return $ show code'
  where formatter = Default.create

parse ::
      Maybe FilePath -> String -> Result.Result Concrete.Commented
parse maybeFile source
  = case result of
        Source.ParseFailed position message -> Result.fatalError $
                                                 Error.ParseError position message
        Source.ParseOk (root, comments) -> return $
                                             Concrete.createCommented root comments
  where result = Source.parseFileContentsWithComments mode source
        mode
          = case maybeFile of
                Nothing -> Source.defaultParseMode
                Just file -> Source.defaultParseMode{Source.parseFilename = file}
