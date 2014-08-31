module Language.Haskell.Formatter.Process.FormatActualCode
       (formatActualCode) where
import qualified Language.Haskell.Formatter.Process.Code as Code
import qualified Language.Haskell.Formatter.Result as Result
import qualified Language.Haskell.Formatter.Source as Source
import qualified Language.Haskell.Formatter.Toolkit.Visit as Visit

formatActualCode ::
                 Code.LocatableCommentableCode -> Result.Result Code.LocatableCode
formatActualCode locatableCommentable
  = case parseResult of
        Source.ParseFailed _ _ -> Result.fatalAssertionError message
          where message = "Formatting the actual code failed to parse."
        Source.ParseOk formattedButChanged -> case maybeLocatable' of
                                                  Nothing -> Result.fatalAssertionError message
                                                    where message
                                                            = "Formatting the actual code failed to zip."
                                                  Just locatable' -> return locatable'
          where maybeLocatable'
                  = Visit.halfZipWith (flip const) locatable formattedButChanged
  where parseResult
          = Source.parseFileContents $ Source.prettyPrint locatable
        locatable = Code.dropComments locatableCommentable
