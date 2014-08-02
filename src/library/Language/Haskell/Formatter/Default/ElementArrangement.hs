module Language.Haskell.Formatter.Default.ElementArrangement
       (arrangeElements) where
import qualified Language.Haskell.Formatter.Code.Concrete
       as Concrete
import qualified Language.Haskell.Formatter.Code.Source as Source

arrangeElements :: Concrete.Commentless -> Concrete.Commentless
arrangeElements commentless
  = commentless{Concrete.commentlessRoot =
                  Source.fromParseResult .
                    Source.parseFileContents . Source.prettyPrint
                    $ Concrete.commentlessRoot commentless}
