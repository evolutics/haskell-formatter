module Evolutics.SourceTree (SourceTree(..), toElementOnly) where
import qualified Language.Haskell.Exts.Annotated as Exts
import qualified Evolutics.AttachedComment as AttachedComment

data SourceTree = SourceTree (Exts.Module
                                (Exts.SrcSpanInfo, [AttachedComment.AttachedComment]))

instance Show SourceTree where
        show sourceTree = Exts.exactPrint (toElementOnly sourceTree) []

toElementOnly :: SourceTree -> Exts.Module Exts.SrcSpanInfo
toElementOnly (SourceTree element) = fmap fst element
