module Evolutics.Code.ConcreteCommentless (ConcreteCommentless(..))
       where
import qualified Language.Haskell.Exts.Annotated as Exts

data ConcreteCommentless = ConcreteCommentless (Exts.Module
                                                  Exts.SrcSpanInfo)
