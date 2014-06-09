module Evolutics.Annotated.Traversable () where
import Control.Applicative
import Language.Haskell.Exts.Annotated
import Evolutics.Tools.MyTraversable

instance Traversable Name where
        traverse f (Ident l s) = Ident <$> f l <*> pure s
        traverse f (Symbol l s) = Symbol <$> f l <*> pure s
