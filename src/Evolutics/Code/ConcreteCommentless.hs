module Evolutics.Code.ConcreteCommentless
       (ConcreteCommentless, root, create) where
import qualified Language.Haskell.Exts.Annotated as Exts

data ConcreteCommentless = ConcreteCommentless{root ::
                                               Exts.Module Exts.SrcSpanInfo}

create :: Exts.Module Exts.SrcSpanInfo -> ConcreteCommentless
create root = ConcreteCommentless{root = root}
