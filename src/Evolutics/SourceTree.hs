module Evolutics.SourceTree
       (SourceTree(element, comments), createSourceTree) where
import qualified Language.Haskell.Exts.Annotated as Exts

data SourceTree = SourceTree{element ::
                             Exts.Module Exts.SrcSpanInfo,
                             comments :: [Exts.Comment]}

instance Show SourceTree where
        show SourceTree{element = element, comments = comments}
          = Exts.exactPrint element comments

createSourceTree ::
                 Exts.Module Exts.SrcSpanInfo -> [Exts.Comment] -> SourceTree
createSourceTree element comments
  = SourceTree{element = element, comments = comments}
