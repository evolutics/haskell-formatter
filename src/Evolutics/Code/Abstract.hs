module Evolutics.Code.Abstract (Code, Comment, create) where
import qualified Language.Haskell.Exts.Annotated as Exts

data Code = Code{root :: Exts.Module [Comment]}

data Comment = Comment{displacement :: Displacement,
                       isMultiLine :: Bool, content :: String}

data Displacement = Before
                  | After

create :: Exts.Module [Comment] -> Code
create root = Code{root = root}
