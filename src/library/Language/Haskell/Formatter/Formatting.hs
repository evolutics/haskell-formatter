module Language.Haskell.Formatter.Formatting (formatSource) where
import qualified Language.Haskell.Formatter.Code.Concrete
       as Concrete
import qualified Language.Haskell.Formatter.Code.Source as Source
import qualified Language.Haskell.Formatter.Default.Formatter
       as Default
import qualified Language.Haskell.Formatter.Error as Error
import qualified Language.Haskell.Formatter.Formatter as Formatter

formatSource ::
             Maybe FilePath -> String -> Either Error.Error String
formatSource maybeFile source
  = do code <- parse maybeFile source
       code' <- Formatter.formatCode formatter code
       Right $ show code'
  where formatter = Default.create

parse ::
      Maybe FilePath -> String -> Either Error.Error Concrete.Commented
parse maybeFile source
  = case result of
        Source.ParseFailed position message -> Left $
                                                 Error.ParseError position message
        Source.ParseOk (root, comments) -> Right $
                                             Concrete.createCommented root comments
  where result = Source.parseFileContentsWithComments mode source
        mode
          = case maybeFile of
                Nothing -> Source.defaultParseMode
                Just file -> Source.defaultParseMode{Source.parseFilename = file}
