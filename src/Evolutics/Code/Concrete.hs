module Evolutics.Code.Concrete
       (Commented, commentedRoot, comments, Commentless, commentlessRoot,
        createCommented, createCommentless, dropComments, commentPortion)
       where
import qualified Language.Haskell.Exts.Annotated as Exts

data Commented = Commented{commentedRoot ::
                           Exts.Module Exts.SrcSpanInfo,
                           comments :: [Exts.Comment]}

data Commentless = Commentless{commentlessRoot ::
                               Exts.Module Exts.SrcSpanInfo}

instance Show Commented where
        show Commented{commentedRoot = root, comments = comments}
          = Exts.exactPrint root comments

createCommented ::
                Exts.Module Exts.SrcSpanInfo -> [Exts.Comment] -> Commented
createCommented root comments
  = Commented{commentedRoot = root, comments = comments}

createCommentless :: Exts.Module Exts.SrcSpanInfo -> Commentless
createCommentless root = Commentless{commentlessRoot = root}

dropComments :: Commented -> Commentless
dropComments Commented{commentedRoot = root}
  = createCommentless root

commentPortion :: Exts.Comment -> Exts.SrcSpan
commentPortion (Exts.Comment _ portion _) = portion
