module Evolutics.Transformations.ElementArrangement
       (arrangeElements) where
import qualified Evolutics.Code.Concrete as Concrete
import qualified Evolutics.Code.Source as Source

arrangeElements :: Concrete.Commentless -> Concrete.Commentless
arrangeElements commentless
  = commentless{Concrete.commentlessRoot =
                  Source.fromParseResult .
                    Source.parseFileContents . Source.prettyPrint
                    $ Concrete.commentlessRoot commentless}
