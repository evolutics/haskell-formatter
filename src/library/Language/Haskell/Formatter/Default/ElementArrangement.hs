module Language.Haskell.Formatter.Default.ElementArrangement
       (arrangeElements) where
import qualified Language.Haskell.Formatter.Code.Concrete
       as Concrete
import qualified Language.Haskell.Formatter.Code.Merged as Merged
import qualified Language.Haskell.Formatter.Code.Source as Source
import qualified Language.Haskell.Formatter.Result as Result
import qualified Language.Haskell.Formatter.Toolkit.Visit as Visit

arrangeElements ::
                Merged.Code -> Result.Result Concrete.Commentless
arrangeElements merged
  = case parseResult of
        Source.ParseFailed _ _ -> Result.fatalAssertionError
                                    "The element arrangement failed to parse."
        Source.ParseOk arrangedButChanged -> case maybeArrangedOriginal of
                                                 Nothing -> Result.fatalAssertionError
                                                              "The element arrangement failed to merge."
                                                 Just arrangedOriginal -> return
                                                                            code{Concrete.commentlessRoot
                                                                                   =
                                                                                   arrangedOriginal}
          where maybeArrangedOriginal
                  = Visit.halfZipWith (flip const) original arrangedButChanged
  where parseResult
          = Source.parseFileContents $ Source.prettyPrint original
        original = Concrete.commentlessRoot code
        code = Merged.makeCommentless merged
