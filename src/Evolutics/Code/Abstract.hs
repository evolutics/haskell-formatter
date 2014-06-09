module Evolutics.Code.Abstract (Code, Comment, createCode) where
import qualified Language.Haskell.Exts.Annotated as Exts
import qualified Evolutics.Code.Concrete as Concrete

data Code = Code{root :: Exts.Module [Comment]}

data Comment = Comment{displacement :: Displacement,
                       isMultiLine :: Bool, content :: String}

data Displacement = Before
                  | After

createCode :: Exts.Module [Comment] -> Code
createCode root = Code{root = root}
