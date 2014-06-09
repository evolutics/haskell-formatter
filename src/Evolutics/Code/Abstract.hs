module Evolutics.Code.Abstract
       (Code, Comment, Displacement(..), createCode, createComment) where
import qualified Language.Haskell.Exts.Annotated as Exts
import qualified Evolutics.Code.Concrete as Concrete

data Code = Code{root :: Exts.Module [Comment]}

data Comment = Comment{commentDisplacement :: Displacement,
                       isCommentMultiLine :: Bool, commentContent :: String}

data Displacement = Before
                  | After

createCode :: Exts.Module [Comment] -> Code
createCode root = Code{root = root}

createComment :: Displacement -> Bool -> String -> Comment
createComment displacement isMultiLine content
  = Comment{commentDisplacement = displacement,
            isCommentMultiLine = isMultiLine, commentContent = content}
