module Evolutics.Transformations.ElementArrangement
       (arrangeElements) where
import qualified Evolutics.Code.Concrete as Concrete
import qualified Evolutics.Code.Core as Core

arrangeElements :: Concrete.Commentless -> Concrete.Commentless
arrangeElements
  = Concrete.createCommentless .
      Core.fromParseResult .
        Core.parseFileContents .
          Core.prettyPrint . Concrete.commentlessRoot
