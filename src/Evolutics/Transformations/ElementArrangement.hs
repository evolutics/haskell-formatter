module Evolutics.Transformations.ElementArrangement
       (arrangeElements) where
import qualified Language.Haskell.Exts.Annotated as Exts
import qualified Evolutics.Code.Concrete.Commentless as Commentless

arrangeElements ::
                Commentless.Commentless -> Commentless.Commentless
arrangeElements
  = Commentless.create .
      Exts.fromParseResult .
        Exts.parseFileContents . Exts.prettyPrint . Commentless.root
