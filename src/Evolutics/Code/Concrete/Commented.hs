module Evolutics.Code.ConcreteCommented
       (ConcreteCommented, root, comments, create, dropComments) where
import qualified Language.Haskell.Exts.Annotated as Exts
import qualified Evolutics.Code.ConcreteCommentless
       as ConcreteCommentless

data ConcreteCommented = ConcreteCommented{root ::
                                           Exts.Module Exts.SrcSpanInfo,
                                           comments :: [Exts.Comment]}

instance Show ConcreteCommented where
        show ConcreteCommented{root = root, comments = comments}
          = Exts.exactPrint root comments

create ::
       Exts.Module Exts.SrcSpanInfo -> [Exts.Comment] -> ConcreteCommented
create root comments
  = ConcreteCommented{root = root, comments = comments}

dropComments ::
             ConcreteCommented -> ConcreteCommentless.ConcreteCommentless
dropComments ConcreteCommented{root = root}
  = ConcreteCommentless.create root
