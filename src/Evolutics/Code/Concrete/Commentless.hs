module Evolutics.Code.Concrete.Commentless
       (Commentless, root, create) where
import qualified Language.Haskell.Exts.Annotated as Exts

data Commentless = Commentless{root ::
                               Exts.Module Exts.SrcSpanInfo}

create :: Exts.Module Exts.SrcSpanInfo -> Commentless
create root = Commentless{root = root}
