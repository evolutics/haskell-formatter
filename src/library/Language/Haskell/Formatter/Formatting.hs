module Language.Haskell.Formatter.Formatting (format) where
import qualified Language.Haskell.Formatter.Code.Concrete
       as Concrete
import qualified Language.Haskell.Formatter.Code.Source as Source
import qualified Language.Haskell.Formatter.Default.Formatter
       as Default
import qualified Language.Haskell.Formatter.Error as Error
import qualified Language.Haskell.Formatter.Formatter as Formatter
import qualified Language.Haskell.Formatter.Result as Result
import qualified Language.Haskell.Formatter.Toolkit.StreamName
       as StreamName

format ::
       StreamName.StreamName -> String -> Either Error.Error String
format stream = Result.toEither . tryFormat stream

tryFormat ::
          StreamName.StreamName -> String -> Result.Result String
tryFormat stream source
  = do code <- parse stream source
       code' <- Formatter.formatCode formatter code
       return $ show code'
  where formatter = Default.create

parse ::
      StreamName.StreamName -> String -> Result.Result Concrete.Commented
parse stream source
  = case result of
        Source.ParseFailed position message -> Result.fatalError $
                                                 Error.ParseError position message
        Source.ParseOk (root, comments) -> return $
                                             Concrete.createCommented root comments
  where result = Source.parseFileContentsWithComments mode source
        mode = Source.defaultParseMode{Source.parseFilename = show stream}
