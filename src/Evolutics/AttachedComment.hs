module Evolutics.AttachedComment (AttachedComment) where
import qualified Language.Haskell.Exts.Annotated as Exts

data AttachedComment = AttachedComment Displacement Exts.Comment

data Displacement = Before
                  | After
