module Evolutics.Transformations.ElementArrangement
       (arrangeElements) where
import qualified Language.Haskell.Exts.Annotated as Exts
import qualified Evolutics.Code.ConcreteCommentless
       as ConcreteCommentless

arrangeElements ::
                ConcreteCommentless.ConcreteCommentless ->
                  ConcreteCommentless.ConcreteCommentless
arrangeElements
  = ConcreteCommentless.create .
      Exts.fromParseResult .
        Exts.parseFileContents .
          Exts.prettyPrint . ConcreteCommentless.root
