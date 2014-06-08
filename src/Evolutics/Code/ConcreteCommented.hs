module Evolutics.Code.ConcreteCommented (ConcreteCommented(..))
       where
import qualified Language.Haskell.Exts.Annotated as Exts

data ConcreteCommented = ConcreteCommented (Exts.Module
                                              Exts.SrcSpanInfo)
                                           [Exts.Comment]
