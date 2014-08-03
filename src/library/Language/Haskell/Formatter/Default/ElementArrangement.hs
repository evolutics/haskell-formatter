module Language.Haskell.Formatter.Default.ElementArrangement
       (arrangeElements) where
import qualified Language.Haskell.Formatter.Code.Concrete
       as Concrete
import qualified Language.Haskell.Formatter.Code.Merged as Merged
import qualified Language.Haskell.Formatter.Code.Source as Source
import qualified Language.Haskell.Formatter.Error as Error
import qualified Language.Haskell.Formatter.Result as Result

arrangeElements ::
                Merged.Code -> Result.Result Concrete.Commentless
arrangeElements merged
  = case result of
        Source.ParseFailed _ _ -> Result.fatalError
                                    Error.ElementArrangementError
        Source.ParseOk code' -> return
                                  code{Concrete.commentlessRoot = code'}
  where result
          = Source.parseFileContents . Source.prettyPrint $
              Concrete.commentlessRoot code
        code = Merged.makeCommentless merged
