module Evolutics.Transformations.ElementArrangement
       (arrangeElements) where
import qualified Language.Haskell.Exts.Annotated as Exts
import qualified Evolutics.Code.Concrete as Concrete

arrangeElements :: Concrete.Commentless -> Concrete.Commentless
arrangeElements
  = Concrete.createCommentless .
      Exts.fromParseResult .
        Exts.parseFileContents .
          Exts.prettyPrint . Concrete.commentlessRoot
