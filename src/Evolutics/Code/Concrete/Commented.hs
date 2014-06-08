module Evolutics.Code.Concrete.Commented
       (Commented, root, comments, create, dropComments) where
import qualified Language.Haskell.Exts.Annotated as Exts
import qualified Evolutics.Code.Concrete.Commentless as Commentless

data Commented = Commented{root :: Exts.Module Exts.SrcSpanInfo,
                           comments :: [Exts.Comment]}

instance Show Commented where
        show Commented{root = root, comments = comments}
          = Exts.exactPrint root comments

create ::
       Exts.Module Exts.SrcSpanInfo -> [Exts.Comment] -> Commented
create root comments = Commented{root = root, comments = comments}

dropComments :: Commented -> Commentless.Commentless
dropComments Commented{root = root} = Commentless.create root
