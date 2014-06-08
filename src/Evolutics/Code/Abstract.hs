module Evolutics.Code.Abstract (Abstract(..)) where
import qualified Language.Haskell.Exts.Annotated as Exts

data Abstract = Abstract (Exts.Module [AbstractComment])

data AbstractComment = AbstractComment Displacement Bool String

data Displacement = Before
                  | After
