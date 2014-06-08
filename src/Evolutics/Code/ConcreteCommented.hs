module Evolutics.Code.ConcreteCommented
       (ConcreteCommented, root, comments, create) where
import qualified Language.Haskell.Exts.Annotated as Exts

data ConcreteCommented = ConcreteCommented{root ::
                                           Exts.Module Exts.SrcSpanInfo,
                                           comments :: [Exts.Comment]}

create ::
       Exts.Module Exts.SrcSpanInfo -> [Exts.Comment] -> ConcreteCommented
create root comments
  = ConcreteCommented{root = root, comments = comments}
